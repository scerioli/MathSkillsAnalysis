## =========================================================================== #
####  ANALYSIS OF GLOBAL PREFERENCES with ROBUST LINEAR REGRESSION and GDI  ####
# ============================================================================ #
# This analysis is the extended study done by using additional (using the Gender 
# Development Index), and making use of the robust linear regression instead of 
# OLS for the whole replication analysis.


# ===================================== #
#### 0. LOAD LIBRARIES AND SETH PATH ####
# ===================================== #
# Set the path
setwd("C:/Users/ceriosar/OneDrive - Mars Inc/Desktop/Private Sara/Global-Preferences-Survey/")

# Source helper functions
source("ReproductionAnalysis/functions/helper_functions/SourceFunctions.r")
SourceFunctions(path = "ReproductionAnalysis/functions/")
SourceFunctions(path = "ReproductionAnalysis/functions/helper_functions/")
SourceFunctions(path = "ExtendedAnalysis/functions/")

# Load libraries
LoadRequiredLibraries()


# ========================= #
#### 1. PREPARE THE DATA ####
# ========================= #

# 1.1 Add the new data set
# ---------------------- #
GDI_index <- CreateGDIindex()

# 1.2 Load the data
# --------------- #
data_all <- LoadData()
data_all$GDI_index <- GDI_index
data_all <- PrepareData_new(data_all)

dt_data <- as.data.table(data_all$data)
dt_data <- dt_data[!is.na(subj_math_skills)]

ggplot(dt_data[country == "Kazakhstan"]) +
  geom_density(aes(x = subj_math_skills, fill = factor(gender)), alpha = 0.5)

# Define the total number of people in the country for each gender
dt_data[, nTotGender := .N, by = c("country", "gender")]
# Define the total number of people per country and gender in each category of
# subjective math skills
dt_data[, nCategorySubjMathSkill := .N, by = c("country", "gender", "subj_math_skills")]
# Calculate the percentage in each category to avoid differences due to different
# number of total participamts
dt_data[, percSubMathSkills := nCategorySubjMathSkill/nTotGender, by = "country"]

dt_sms <- unique(dt_data[, .(country, gender, subj_math_skills, percSubMathSkills)])
dt_sms <- dt_sms[order(country, subj_math_skills, gender)]

# Define the relative difference between females and males ( 1 - 0 ):
# - Negative number: more women in that category
# - Positive number: more men in that category
dt_sms[, diffPercSMS := percSubMathSkills - shift(percSubMathSkills), 
       by = c("country", "subj_math_skills")]

dt_sms_analysis <- dt_sms[!is.na(diffPercSMS), .(country, subj_math_skills, diffPercSMS)]


# TODO: Should I group per categories as "lowest", "middle", and "high"?
# Let's check the lowest category

ggplot(dt_sms_analysis[subj_math_skills == 0]) +
  geom_point(aes(x = reorder(x = country, diffPercSMS), y = diffPercSMS)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  labs(title = "Subjective Math Skills = 0") + 
  xlab("Country") + 
  ylab("Females - Males (%)") +
  geom_hline(yintercept = 0)

ggplot(dt_sms_analysis[subj_math_skills == 10]) +
  geom_point(aes(x = reorder(x = country, diffPercSMS), y = diffPercSMS)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  labs(title = "Subjective Math Skills = 10") + 
  xlab("Country") + 
  ylab("Females - Males (%)") +
  geom_hline(yintercept = 0)
  

ggplot(dt_sms_analysis[subj_math_skills == 5]) +
  geom_point(aes(x = reorder(x = country, diffPercSMS), y = diffPercSMS)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  labs(title = "Subjective Math Skills = 5") + 
  xlab("Country") + 
  ylab("Females - Males (%)") +
  geom_hline(yintercept = 0)

ggplot(dt_sms_analysis[subj_math_skills == 4]) +
  geom_point(aes(x = reorder(x = country, diffPercSMS), y = diffPercSMS)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  labs(title = "Subjective Math Skills = 4") + 
  xlab("Country") + 
  ylab("Females - Males (%)") +
  geom_hline(yintercept = 0)


ggplot(dt_sms_analysis) +
  geom_point(aes(x = country, y = diffPercSMS, 
                 col = ifelse(diffPercSMS > 0, "female > male", "male > female"))) +
  facet_wrap(~subj_math_skills) +
  theme_bw() +
  labs(col = "Difference in skills") + 
  theme(axis.text.x = element_blank()) +
  xlab("Country") + 
  ylab("Females - Males (%)") +
  geom_hline(yintercept = 0)

