# =============================================================================
# Occupation type.R
# Cleans the PLFS 2025 data, builds the informality variables, and writes the
# CSV files the Closeread story reads from data/.
# =============================================================================

library(dplyr)
library(tidyr)
library(haven)
library(readxl)


# Raw data location (stays in OneDrive) -----------------------------------

raw_dir <- "C:/Users/rzs0133/OneDrive - Auburn University/Desktop_Auburn Department Computer/PHD dissertation/Climate Stress/PLFS"


# Read and merge the data -------------------------------------------------

plfs_personv1 <- read_dta(file.path(raw_dir, "cperv12025.dta"))
plfs_hh       <- read_dta(file.path(raw_dir, "chhv12025.dta"))

plfs_indv <- plfs_personv1 %>%
  haven::zap_labels() %>%
  mutate(
    strata_id = paste(st, sec, bstrm, strm, grp, sstrm, sss, sep = "_"),
    hh_id = paste(sprintf("%02d", as.numeric(month)),
                  visit, sec, mfsu, sss, ssu, sep = "_"),
    wt = mult/100
  )

plfs_hh <- plfs_hh %>%
  mutate(hh_id = paste(sprintf("%02d", as.numeric(month)),
                       visit, sec, mfsu, sss, ssu, sep = "_"))

plfs_master <- plfs_indv %>%
  left_join(plfs_hh %>% select(hh_id, sg),    # sg = social group
            by = "hh_id",
            relationship = "many-to-one")


# State and district names ------------------------------------------------

states <- read_excel(file.path(raw_dir, "Indian_States_and_UTs_CodeName.xlsx")) %>%
  rename(st = `State Code`, state = `State/UT Name`)

Bstrm_file <- read_excel(file.path(raw_dir, "Bstrm_file.xlsx"),
                         sheet = "bstrm data") %>%
  distinct(st = stnew, bstrm, unit_name = Bstrm_name)

plfs_master <- plfs_master %>%
  left_join(states, by = "st") %>%
  left_join(Bstrm_file, by = c("st", "bstrm")) %>%
  mutate(
    unit_type = ifelse(substr(bstrm, 1,1) == "D", "District", "District group (NSS Region)"),
    unit_name = ifelse(bstrm == "N999", "Uninhabited stratum", unit_name)
  ) %>%
  arrange(state, bstrm)


# Informal employment and informal enterprise variables -------------------

plfs_inf <- plfs_master %>%
  mutate(
    ind_pas = na_if(trimws(ind_pas), ""),
    ocu_pas = na_if(trimws(ocu_pas), ""),
    status = as.integer(pas),
    etyp = as.integer(etyp_pas),
    ssec = as.integer(ssec_pas),
    worker = status %in% 11:51,
    nic2 = as.integer(substr(ind_pas, 1, 2)),
    nic3 = substr(ind_pas, 1, 3),
    nco1 = substr(ocu_pas, 1, 1),
    # crop agriculture
    agegc = nic3 %in% c("011", "012", "013", "015"),
    # sectors of the economy
    sector3 = case_when(
      nic2 <= 3    ~ "Primary",
      nic2 <= 43   ~ "Secondary",
      !is.na(nic2) ~ "Tertiary"
    ),
    # industry
    sector8 = case_when(
      nic2 <= 3                 ~ "Agriculture",
      nic2 <= 9                 ~ "Mining & quarrying",
      nic2 <= 33                ~ "Manufacturing",
      nic2 <= 39                ~ "Electricity, gas, water",
      nic2 <= 43                ~ "Construction",
      nic2 %in% c(45:47, 55:56) ~ "Trade, hotels & restaurants",
      nic2 %in% c(49:53, 58:63) ~ "Transport, storage & communication",
      !is.na(nic2)              ~ "Other services"
    ),
    # occupation
    occ5 = case_when(
      nco1 %in% c("1", "2", "3") ~ "Managers & professionals",
      nco1 %in% c("4", "5")      ~ "Clerical, sales & service",
      nco1 == "6"                ~ "Skilled agriculture",
      nco1 %in% c("7", "8")      ~ "Craft & machine operators",
      nco1 == "9"                ~ "Elementary (manual labor)",
      nco1 == "0"                ~ "Armed forces"
    ),
    # status in employment
    emp_status_grp = case_when(
      status %in% c(11, 12) ~ "Self-employed",
      status == 21          ~ "Unpaid family helper",
      status == 31          ~ "Regular wage/salaried",
      status %in% c(41, 51) ~ "Casual labor"
    ),
    # enterprise type
    enterprise = case_when(
      !worker                  ~ NA_character_,
      agegc                    ~ "Crop agriculture",
      etyp %in% 1:4            ~ "Informal sector",
      etyp %in% c(5:8, 10, 11) ~ "Formal sector",
      etyp == 12               ~ "Household (domestic work)",
      TRUE                     ~ "Other/unknown"
    ),
    # informal sector (only non-crop workers)
    informal_sector = case_when(
      enterprise == "Informal sector"                                 ~ TRUE,
      enterprise %in% c("Formal sector", "Household (domestic work)") ~ FALSE,
      TRUE                                                            ~ NA
    ),
    # informal employment
    informal_emp = case_when(
      !worker                       ~ NA,
      status %in% c(21, 41, 51)     ~ TRUE,
      status == 31 & is.na(ssec)    ~ NA,
      status == 31                  ~ ssec %in% c(8, 9),
      status %in% c(11, 12) & agegc ~ TRUE,
      status %in% c(11, 12)         ~ enterprise %in% c("Informal sector", "Other/unknown"),
      TRUE                          ~ NA
    ),
    # sex, social group and age group
    sex_lbl = case_when(
      sex == "1" ~ "Male",
      sex == "2" ~ "Female",
      sex == "3" ~ "Transgender"
    ),
    social_grp = case_when(
      sg == "1" ~ "ST",
      sg == "2" ~ "SC",
      sg == "3" ~ "OBC",
      sg == "9" ~ "Others"
    ),
    age_grp = cut(age, breaks = c(-Inf, 14, 29, 44, 59, Inf),
                  labels = c("0-14", "15-29", "30-44", "45-59", "60+")),
    # 0/1 so the mean equals the share of workers who are informal
    informal_emp = as.numeric(informal_emp),
    informal_sector = as.numeric(informal_sector),
    # enterprise type labels for the informal sector
    etyp_lbl = case_when(
      etyp == 1 ~ "Proprietary (male)",
      etyp == 2 ~ "Proprietary (female)",
      etyp == 3 ~ "Partnership (same household)",
      etyp == 4 ~ "Partnership (different households)"
    )
  )


# Data for the story ------------------------------------------------------

plfs_svy <- plfs_inf %>%
  select(mfsu, strata_id, wt, worker, informal_emp, informal_sector, enterprise,
         sector8, occ5, emp_status_grp, etyp_lbl, sex_lbl, social_grp, age_grp,
         status, ssec, state, unit_name, unit_type)

workers_df <- plfs_svy %>% filter(worker)
infemp_df  <- workers_df %>% filter(informal_emp == 1)
infsec_df  <- workers_df %>% filter(informal_sector == 1)

sum(workers_df$wt) / 1e6   # total workers, millions


# 1. Composition: share of the group in each category ---------------------

comp <- function(data, var, storyline) {
  data %>%
    filter(!is.na(.data[[var]])) %>%
    count(category = .data[[var]], wt = wt) %>%
    mutate(storyline = storyline,
           dimension = var,
           share     = 100 * n / sum(n),
           workers_m = n / 1e6) %>%
    select(storyline, dimension, category, share, workers_m)
}

composition <- bind_rows(
  lapply(c("enterprise", "sector8", "occ5", "emp_status_grp",
           "sex_lbl", "social_grp", "age_grp"),
         function(v) comp(infemp_df, v, "informal_emp")),
  lapply(c("sector8", "etyp_lbl", "emp_status_grp",
           "sex_lbl", "social_grp", "age_grp"),
         function(v) comp(infsec_df, v, "informal_sector"))
)


# 2. Incidence: share of each category that is informal -------------------

incid <- function(data, var, outcome) {
  data %>%
    filter(!is.na(.data[[var]]), !is.na(.data[[outcome]])) %>%
    group_by(category = .data[[var]]) %>%
    summarise(share     = 100 * weighted.mean(.data[[outcome]], wt),
              workers_m = sum(wt) / 1e6,
              .groups   = "drop") %>%
    mutate(outcome = outcome, dimension = var) %>%
    select(outcome, dimension, category, share, workers_m)
}

incidence <- bind_rows(
  lapply(c("enterprise", "sector8", "occ5", "emp_status_grp",
           "sex_lbl", "social_grp", "age_grp"),
         function(v) incid(workers_df, v, "informal_emp")),
  lapply(c("sector8", "emp_status_grp", "sex_lbl", "social_grp", "age_grp"),
         function(v) incid(workers_df, v, "informal_sector"))
)


# 2b. Occupation x social group --------------------------------------------
# For each occupation and social group: workers, informal workers, survey
# respondents, the group's share in each occupation (chart A) and the
# informality rate (chart B).

occ_social <- workers_df %>%
  filter(!is.na(occ5), occ5 != "Armed forces", !is.na(social_grp)) %>%
  group_by(occ5, social_grp) %>%
  summarise(workers  = sum(wt),                        # weighted workers
            informal = sum(wt[informal_emp == 1]),     # weighted informal workers
            n_obs = n(),                               # survey respondents
            .groups = "drop") %>%
  group_by(social_grp) %>%
  mutate(share_of_group = 100 * workers/ sum(workers)) %>% # Chart A
  ungroup() %>%
  mutate(informal_rate = 100 * informal/ workers,           # Chart B
         workers_m     = workers/ 1e6,
         informal_m    = informal/ 1e6) %>%
  select(occ5, social_grp, workers_m, informal_m, share_of_group, informal_rate, n_obs)

# 3. Joint table and dots (cumulative rounding) ---------------------------

dot_size <- 1e6   # 1 dot = 1 million workers

joint <- workers_df %>%
  count(informal_emp, informal_sector, enterprise, sector8, etyp_lbl,
        age_grp, sex_lbl, social_grp,
        wt = wt, name = "Freq") %>%
  filter(Freq > 0)

dots <- joint %>%
  arrange(desc(informal_emp), enterprise, sector8, age_grp, sex_lbl, social_grp) %>%
  mutate(cum    = round(cumsum(Freq) / dot_size),   # running total, in whole dots
         n_dots = cum - lag(cum, default = 0)) %>%  # dots added by this combination
  select(-cum, -Freq) %>%
  uncount(n_dots) %>%                               # one row per dot
  mutate(dot_id = row_number())                     # ID so the chart can track each dot

nrow(dots)

# check: dot shares vs exact shares (should be within about half a point)
dots %>% filter(informal_emp == 1) %>% count(enterprise) %>% mutate(pct = round(100 * n / sum(n), 1))
composition %>% filter(storyline == "informal_emp", dimension == "enterprise")


# 4. Headline numbers -----------------------------------------------------

headline <- tibble(
  measure = c("workers_m", "informal_emp_share", "informal_emp_m",
              "informal_sector_share", "informal_sector_m"),
  value = c(
    sum(workers_df$wt) / 1e6,
    100 * weighted.mean(workers_df$informal_emp, workers_df$wt, na.rm = TRUE),
    sum(infemp_df$wt) / 1e6,
    100 * weighted.mean(workers_df$informal_sector, workers_df$wt, na.rm = TRUE),
    sum(infsec_df$wt) / 1e6
  )
)


# 5. Save into the project's data folder ----------------------------------

write.csv(headline,    "data/headline.csv",    row.names = FALSE)
write.csv(composition, "data/composition.csv", row.names = FALSE, na = "")
write.csv(incidence,   "data/incidence.csv",   row.names = FALSE, na = "")
write.csv(joint,       "data/joint.csv",       row.names = FALSE, na = "")
write.csv(dots,        "data/dots.csv",        row.names = FALSE, na = "")
write.csv(occ_social,  "data/occ_social.csv",  row.names = FALSE, na = "")
