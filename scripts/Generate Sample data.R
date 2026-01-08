# --------------------------------------------------
# Synthetic Integrated Health Dataset
# Purpose: Generate sample data for Power BI book
# Domains: ANC, HIV, ART, Malaria, TB, NCDs
# Note: Data intentionally includes quality issues
# --------------------------------------------------


set.seed(123)

library(dplyr)
library(lubridate)
library(stringr)

# -------------------------
# Dimensions 
# -------------------------
dim_patient <- tibble(
  patient_id = 1:3500,
  sex = sample(c("Male", "Female"), 3500, replace = TRUE),
  age = sample(0:80, 3500, replace = TRUE),
  hiv_status = sample(c("Positive", "Negative", "Unknown"),
                      3500, replace = TRUE,
                      prob = c(0.05, 0.8, 0.15))
) %>%
  mutate(
    age_group = case_when(
      age < 5 ~ "<5",
      age < 15 ~ "5–14",
      age < 50 ~ "15–49",
      TRUE ~ "50+"
    ),
    pregnant = if_else(sex == "Female" & age >= 15 & age <= 49,
                       sample(c("Yes", "No"), n(), replace = TRUE, prob = c(0.25, 0.75)),
                       "No")
  )

dim_facility <- tibble(
  facility_id = 1:60,
  facility_name = paste("Facility", facility_id),
  facility_type = sample(c("Primary", "Secondary"), 60, replace = TRUE),
  ownership = sample(c("Public", "Private"), 60, replace = TRUE),
  district = sample(paste("District", 1:12), 60, replace = TRUE),
  state = sample(c("State A", "State B", "State C"), 60, replace = TRUE)
)

dim_date <- tibble(
  visit_date = seq.Date(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
) %>%
  mutate(
    date_id = as.integer(format(visit_date, "%Y%m%d")),
    year = year(visit_date),
    quarter = paste0("Q", quarter(visit_date)),
    month = month(visit_date, label = TRUE),
    epi_week = isoweek(visit_date)
  )

# -------------------------
# Fact table with dirty data
# -------------------------
fact_patient_visits <- tibble(
  visit_id = 1:10000,
  patient_id = sample(dim_patient$patient_id, 10000, replace = TRUE),
  facility_id = sample(dim_facility$facility_id, 10000, replace = TRUE),
  visit_date = sample(dim_date$visit_date, 10000, replace = TRUE),
  
  anc_visit = sample(c("Yes", "No", " yes ", "YES"), 10000, replace = TRUE,
                     prob = c(0.18, 0.72, 0.05, 0.05)),
  
  malaria_tested = sample(c("Yes", "No"), 10000, replace = TRUE, prob = c(0.7, 0.3)),
  tb_screened = sample(c("Yes", "No"), 10000, replace = TRUE, prob = c(0.4, 0.6)),
  hiv_tested = sample(c("Yes", "No"), 10000, replace = TRUE, prob = c(0.5, 0.5)),
  ncd_screened = sample(c("Yes", "No"), 10000, replace = TRUE, prob = c(0.35, 0.65))
) %>%
  mutate(
    malaria_positive = if_else(malaria_tested == "Yes",
                               sample(c("Yes", "No"), n(), replace = TRUE, prob = c(0.3, 0.7)),
                               "No"),
    
    tb_test_done = if_else(tb_screened == "Yes",
                           sample(c("Yes", "No"), n(), replace = TRUE, prob = c(0.8, 0.2)),
                           "No"),
    
    tb_test_date = if_else(
      tb_test_done == "Yes",
      as.character(sample(seq(as.Date("2023-10-01"), as.Date("2024-12-31"), by = "day"), n(), replace = TRUE)),
      NA_character_
    ),
    
    # Introduce bad date formats
    tb_test_date = if_else(runif(n()) < 0.1 & !is.na(tb_test_date),
                           str_replace(tb_test_date, "-", "/"),
                           tb_test_date),
    
    tb_positive = if_else(tb_test_done == "Yes",
                          sample(c("Yes", "No"), n(), replace = TRUE, prob = c(0.1, 0.9)),
                          "No"),
    
    hiv_positive = if_else(hiv_tested == "Yes",
                           sample(c("Yes", "No"), n(), replace = TRUE, prob = c(0.08, 0.92)),
                           "No"),
    
    art_started = if_else(hiv_positive == "Yes",
                          sample(c("Yes", "No"), n(), replace = TRUE, prob = c(0.75, 0.25)),
                          "No"),
    
    art_start_date = if_else(
      art_started == "Yes",
      as.character(sample(seq(as.Date("2019-01-01"), as.Date("2024-12-31"), by = "day"), n(), replace = TRUE)),
      NA_character_
    ),
    
    # Inject logical errors
    art_start_date = if_else(runif(n()) < 0.05 & !is.na(art_start_date),
                             "1900-01-01",
                             art_start_date),
    
    hypertension = if_else(ncd_screened == "Yes",
                           sample(c("Yes", "No"), n(), replace = TRUE, prob = c(0.25, 0.75)),
                           "No"),
    
    diabetes = if_else(ncd_screened == "Yes",
                       sample(c("Yes", "No"), n(), replace = TRUE, prob = c(0.12, 0.88)),
                       "No")
  ) %>%
  left_join(dim_date, by = "visit_date") %>%
  select(-visit_date)

# -------------------------
# ART Regimen logic
# -------------------------

fact_patient_visits <- fact_patient_visits %>%
  mutate(
    art_regimen = if_else(
      art_started == "Yes",
      sample(
        c("TLD", "Tld", "TLD ", "TLE", "ABC-3TC-DTG", "AZT-3TC-NVP", NA_character_),
        n(), replace = TRUE,
        prob = c(0.4, 0.1, 0.05, 0.2, 0.15, 0.05, 0.05)
      ),
      NA_character_
    ),
    
    art_line = if_else(
      art_started == "Yes",
      sample(c("First", "Second", "Third"), n(), replace = TRUE,
             prob = c(0.85, 0.1, 0.05)),
      NA_character_
    ),
    
    regimen_changed = if_else(
      art_started == "Yes",
      sample(c("Yes", "No"), n(), replace = TRUE, prob = c(0.15, 0.85)),
      "No"
    )
  )


# -------------------------
# Write CSVs
# -------------------------
dir.create("assets/data", showWarnings = FALSE)

write.csv(dim_patient, "assets/data/dim_patient.csv", row.names = FALSE)
write.csv(dim_facility, "assets/data/dim_facility.csv", row.names = FALSE)
write.csv(dim_date, "assets/data/dim_date.csv", row.names = FALSE)
write.csv(fact_patient_visits, "assets/data/fact_patient_visits.csv", row.names = FALSE)
