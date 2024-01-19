if("pacman" %in% installed.packages()){
  require(pacman)
} else {
  install.packages("pacman")
  require(pacman)
}


pacman::p_load("devtools", "ggpubr", "ggpmisc", "htmltools", "survival", "BiocManager", "survminer", "readr", "gtsummary", "ggsurvfit", "lubridate", "tidycmprsk", "condSURV", "finalfit", "tidyverse", "ggplot2", "ggiraph","ggiraphExtra","plyr")

devtools::install_github("cardiomoon/ggiraphExtra")

setwd("~/Desktop/Academics and Research/COVID-19 Projects/Tracy Boosted Effect Clalit Health")

#install.packages(c("survival", "survminer"))
#library("survival")
#library("survminer")

clalit_data = read_delim('Clalit_Health_Boosting_Data_for_R_Analysis.csv', delim = ',', col_names = T, guess_max = 9000 )

View(clalit_data)
describe(clalit_data)

head(clalit_data)

############# BOOSTED UNIVARIATE UNDER VARIOUS PERSON YEARS OF RISK FORMULATIONS ################
# Univariate Boosted: Incremental COVID deaths as a function of person years
boosted_py_no_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Boosted_Person_Years, data=clalit_data)
boosted_py_no_adjustment %>%
  summary()

plot <- ggplot(boosted_py_no_adjustment$model) +
  geom_point(aes(x=Boosted_Person_Years, y=Boosted_Incremental_COVID_Deaths)) +
  stat_smooth(method = "lm", aes(x=Boosted_Person_Years, y=Boosted_Incremental_COVID_Deaths), col = "darkred") +
  labs(title="Boosted COVID Deaths vs Boosted Person Years", x="Boosted Person Years", y="Incremental COVID-19 Deaths")

plot + geom_label(aes(x = 0, y = 10), hjust = 0,
                  label = paste("Adj R2 = ",signif(summary(boosted_py_no_adjustment)$adj.r.squared, 5),
                                "\nIntercept =",signif(boosted_py_no_adjustment$coef[[1]],5 ),
                                " \nSlope =",signif(boosted_py_no_adjustment$coef[[2]], 5),
                                " \nP =",signif(summary(boosted_py_no_adjustment)$coef[2,4], 5)))


boosted_py_C19_death_rate_no_adjustment = lm(formula=Boosted_COVID_Death_Rate ~ Boosted_Person_Years, data=clalit_data)
boosted_py_C19_death_rate_no_adjustment %>%
  summary()

plot <- ggplot(boosted_py_C19_death_rate_no_adjustment$model) +
  geom_point(aes(x=Boosted_Person_Years, y=Boosted_COVID_Death_Rate)) +
  stat_smooth(method = "lm", aes(x=Boosted_Person_Years, y=Boosted_COVID_Death_Rate), col = "darkred") +
  labs(title="Boosted COVID Death Rate vs Boosted Person Years", x="Boosted Person Years", y="Incremental COVID-19 Death Rate")

plot + geom_label(aes(x = 7500, y = 0.0002), hjust = 0,
                  label = paste("Adj R2 = ",signif(summary(boosted_py_C19_death_rate_no_adjustment)$adj.r.squared, 5),
                                "\nIntercept =",signif(boosted_py_C19_death_rate_no_adjustment$coef[[1]],5 ),
                                " \nSlope =",signif(boosted_py_C19_death_rate_no_adjustment$coef[[2]], 5),
                                " \nP =",signif(summary(boosted_py_C19_death_rate_no_adjustment)$coef[2,4], 5)))


boosted_py_log_C19_death_rate_no_adjustment = lm(formula=Log_Boosted_COVID_Death_Rate ~ Boosted_Person_Years, data=clalit_data)
boosted_py_log_C19_death_rate_no_adjustment %>%
  summary()

plot <- ggplot(boosted_py_log_C19_death_rate_no_adjustment$model) +
  geom_point(aes(x=Boosted_Person_Years, y=Log_Boosted_COVID_Death_Rate)) +
  stat_smooth(method = "lm", aes(x=Boosted_Person_Years, y=Log_Boosted_COVID_Death_Rate), col = "darkred") +
  labs(title="Boosted COVID Death Rate vs Boosted Person Years", x="Boosted Person Years", y="Log(Incremental COVID Death Rate)")

plot + geom_label(aes(x = 7500, y = -2.6), hjust = 0,
                  label = paste("Adj R2 = ",signif(summary(boosted_py_log_C19_death_rate_no_adjustment)$adj.r.squared, 5),
                                "\nIntercept =",signif(boosted_py_log_C19_death_rate_no_adjustment$coef[[1]],5 ),
                                " \nSlope =",signif(boosted_py_log_C19_death_rate_no_adjustment$coef[[2]], 5),
                                " \nP =",signif(summary(boosted_py_log_C19_death_rate_no_adjustment)$coef[2,4], 5)))

# Now do it against person years "under risk" (meaning unboosted person years)
boosted_py_no_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Boosted_Person_Years_of_Risk, data=clalit_data)
boosted_py_no_adjustment %>%
  summary()

plot <- ggplot(boosted_py_no_adjustment$model) +
  geom_point(aes(x=Boosted_Person_Years_of_Risk, y=Boosted_Incremental_COVID_Deaths)) +
  stat_smooth(method = "lm", aes(x=Boosted_Person_Years_of_Risk, y=Boosted_Incremental_COVID_Deaths), col = "darkred") +
  labs(title="Boosted COVID Deaths vs Person Years Without Booster", x="Person Years Without Booster", y="Incremental COVID Deaths")

plot + geom_label(aes(x = 0, y = 10), hjust = 0,
                  label = paste("Adj R2 = ",signif(summary(boosted_py_no_adjustment)$adj.r.squared, 5),
                                "\nIntercept =",signif(boosted_py_no_adjustment$coef[[1]],5 ),
                                " \nSlope =",signif(boosted_py_no_adjustment$coef[[2]], 5),
                                " \nP =",signif(summary(boosted_py_no_adjustment)$coef[2,4], 5)))


# Univariate Boosted: Cum COVID deaths as a function of cumulative person years
boosted_py_no_adjustment = lm(formula=Boosted_COVID_Deaths_by_Date ~ Boosted_Cum_Person_Years, data=clalit_data)
boosted_py_no_adjustment %>%
  summary()

plot <- ggplot(boosted_py_no_adjustment$model) +
  geom_point(aes(x=Boosted_Cum_Person_Years, y=Boosted_COVID_Deaths_by_Date)) +
  stat_smooth(method = "lm", aes(x=Boosted_Cum_Person_Years, y=Boosted_COVID_Deaths_by_Date), col = "darkred") +
  labs(title="Cumulative Boosted COVID Deaths vs Cumulative Person Years", x="Cumulative Person Years", y="Cumulative COVID Deaths")

plot + geom_label(aes(x = 0, y = 40), hjust = 0,
                  label = paste("Adj R2 = ",signif(summary(boosted_py_no_adjustment)$adj.r.squared, 5),
                                "\nIntercept =",signif(boosted_py_no_adjustment$coef[[1]],5 ),
                                " \nSlope =",signif(boosted_py_no_adjustment$coef[[2]], 5),
                                " \nP =",signif(summary(boosted_py_no_adjustment)$coef[2,4], 5)))


############# UNBOOSTED UNIVARIATE UNDER VARIOUS PERSON YEARS OF RISK FORMULATIONS ################
# Univariate Unboosted: Incremental COVID deaths as a function of person years
unboosted_py_no_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Unboosted_Person_Years, data=clalit_data)
unboosted_py_no_adjustment %>%
  summary()

plot <- ggplot(unboosted_py_no_adjustment$model) +
  geom_point(aes(x=Unboosted_Person_Years, y=Unboosted_Incremental_COVID_Deaths)) +
  stat_smooth(method = "lm", aes(x=Unboosted_Person_Years, y=Unboosted_Incremental_COVID_Deaths), col = "darkred") +
  labs(title="Unboosted COVID Deaths vs Unboosted Person Years", x="Unboosted Person Years", y="Incremental COVID Deaths")

plot + geom_label(aes(x = 5000, y = 0), hjust = 0,
                  label = paste("Adj R2 = ",signif(summary(unboosted_py_no_adjustment)$adj.r.squared, 5),
                                "\nIntercept =",signif(unboosted_py_no_adjustment$coef[[1]],5 ),
                                " \nSlope =",signif(unboosted_py_no_adjustment$coef[[2]], 5),
                                " \nP =",signif(summary(unboosted_py_no_adjustment)$coef[2,4], 5)))

unboosted_py_log_C19_death_rate_no_adjustment = lm(formula=Log_Unboosted_COVID_Death_Rate ~ Unboosted_Person_Years, data=clalit_data)
unboosted_py_log_C19_death_rate_no_adjustment %>%
  summary()

plot <- ggplot(unboosted_py_log_C19_death_rate_no_adjustment$model) +
  geom_point(aes(x=Unboosted_Person_Years, y=Log_Unboosted_COVID_Death_Rate)) +
  stat_smooth(method = "lm", aes(x=Unboosted_Person_Years, y=Log_Unboosted_COVID_Death_Rate), col = "darkred") +
  labs(title="Unboosted COVID Death Rate vs Unboosted Person Years", x="Unboosted Person Years", y="Log(Incremental COVID Death Rate)")

plot + geom_label(aes(x = 5000, y = -2), hjust = 0,
                  label = paste("Adj R2 = ",signif(summary(unboosted_py_log_C19_death_rate_no_adjustment)$adj.r.squared, 5),
                                "\nIntercept =",signif(unboosted_py_log_C19_death_rate_no_adjustment$coef[[1]],5 ),
                                " \nSlope =",signif(unboosted_py_log_C19_death_rate_no_adjustment$coef[[2]], 5),
                                " \nP =",signif(summary(unboosted_py_log_C19_death_rate_no_adjustment)$coef[2,4], 5)))                               " \nSlope =",signif(unboosted_py_no_adjustment$coef[[2]], 5),
                                " \nP =",signif(summary(unboosted_py_no_adjustment)$coef[2,4], 5)))

# Univariate Unboosted: Cum COVID deaths as a function of cumulative person years
unboosted_py_no_adjustment = lm(formula=Unboosted_COVID_Deaths_by_Date ~ Unboosted_Cum_Person_Years, data=clalit_data)
unboosted_py_no_adjustment %>%
  summary()

plot <- ggplot(unboosted_py_no_adjustment$model) +
  geom_point(aes(x=Unboosted_Cum_Person_Years, y=Unboosted_COVID_Deaths_by_Date)) +
  stat_smooth(method = "lm", aes(x=Unboosted_Cum_Person_Years, y=Unboosted_COVID_Deaths_by_Date), col = "darkred") +
  labs(title="Cumulative Unboosted COVID Deaths vs Cumulative Person Years", x="Cumulative Person Years", y="Cumulative COVID Deaths")

plot + geom_label(aes(x = 0, y = 75), hjust = 0,
                  label = paste("Adj R2 = ",signif(summary(unboosted_py_no_adjustment)$adj.r.squared, 5),
                                "\nIntercept =",signif(unboosted_py_no_adjustment$coef[[1]],5 ),
                                " \nSlope =",signif(unboosted_py_no_adjustment$coef[[2]], 5),
                                " \nP =",signif(summary(unboosted_py_no_adjustment)$coef[2,4], 5)))



###########################################################################################################
# Incorporate case rates, deaths, and hospitalizations in Israel over the time period of the study
###########################################################################################################

# Univariate Boosted: Incremental and Cumulative COVID deaths as a function of 6-Day hospitalizations
# Cumulative less meaningful here as the 6-Day hospitalization data is not cumulative over that period
boosted_6_day_hosp_no_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Six_Day_Hospitalizations, data=clalit_data)
boosted_6_day_hosp_no_adjustment %>%
  summary()

boosted_log_C19_Death_Rate_6_day_hosp_no_adjustment = lm(formula=Log_Boosted_COVID_Death_Rate ~ Six_Day_Hospitalizations, data=clalit_data)
boosted_log_C19_Death_Rate_6_day_hosp_no_adjustment %>%
  summary()


plot <- ggplot(boosted_log_C19_Death_Rate_6_day_hosp_no_adjustment$model) +
  geom_point(aes(x=Six_Day_Hospitalizations, y=Log_Boosted_COVID_Death_Rate)) +
  stat_smooth(method = "lm", aes(x=Six_Day_Hospitalizations, y=Log_Boosted_COVID_Death_Rate), col = "darkred") +
  labs(title="Boosted COVID Deaths vs 6-Day Rolling Average Hospitalizations in Israel", x="Rolling 6-Day Average Hospitalizations", y="Log(Incremental COVID-19 Deaths)")

plot + geom_label(aes(x = 0, y = 10), hjust = 0,
                  label = paste("Adj R2 = ",signif(summary(boosted_log_C19_Death_Rate_6_day_hosp_no_adjustment)$adj.r.squared, 5),
                                "\nIntercept =",signif(boosted_log_C19_Death_Rate_6_day_hosp_no_adjustment$coef[[1]],5 ),
                                " \nSlope =",signif(boosted_log_C19_Death_Rate_6_day_hosp_no_adjustment$coef[[2]], 5),
                                " \nP =",signif(summary(boosted_log_C19_Death_Rate_6_day_hosp_no_adjustment)$coef[2,4], 5)))

boosted_6_day_hosp_no_adjustment = lm(formula=Boosted_COVID_Deaths_by_Date ~ Six_Day_Hospitalizations, data=clalit_data)
boosted_6_day_hosp_no_adjustment %>%
  summary()


# Univariate Unboosted: Incremental and cumulative COVID deaths as a function of 6-Day hospitalizations
# Cumulative less meaningful here as the 6-Day hospitalization data is not cumulative over that period
unboosted_6_day_hosp_no_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Six_Day_Hospitalizations, data=clalit_data)
unboosted_6_day_hosp_no_adjustment %>%
  summary()

unboosted_Log_C19_Death_Rate_6_day_hosp_no_adjustment = lm(formula=Log_Unboosted_COVID_Death_Rate ~ Six_Day_Hospitalizations, data=clalit_data)
unboosted_Log_C19_Death_Rate_6_day_hosp_no_adjustment %>%
  summary()


plot <- ggplot(unboosted_6_day_hosp_no_adjustment$model) +
  geom_point(aes(x=Six_Day_Hospitalizations, y=Unboosted_Incremental_COVID_Deaths)) +
  stat_smooth(method = "lm", aes(x=Six_Day_Hospitalizations, y=Unboosted_Incremental_COVID_Deaths), col = "darkred") +
  labs(title="Unboosted COVID Deaths vs 6-Day Rolling Average Hospitalizations in Israel", x="Rolling 6-Day Average Hospitalizations", y="Incremental COVID Deaths")

plot + geom_label(aes(x = 0, y = 10), hjust = 0,
                  label = paste("Adj R2 = ",signif(summary(unboosted_6_day_hosp_no_adjustment)$adj.r.squared, 5),
                                "\nIntercept =",signif(unboosted_6_day_hosp_no_adjustment$coef[[1]],5 ),
                                " \nSlope =",signif(unboosted_6_day_hosp_no_adjustment$coef[[2]], 5),
                                " \nP =",signif(summary(unboosted_6_day_hosp_no_adjustment)$coef[2,4], 5)))

unboosted_6_day_hosp_no_adjustment = lm(formula=Unboosted_COVID_Deaths_by_Date ~ Six_Day_Hospitalizations, data=clalit_data)
unboosted_6_day_hosp_no_adjustment %>%
  summary()


# Univariate Boosted: Incremental and Cumulative COVID deaths as a function of 6-Day rolling average weekly admissions
# Cumulative less meaningful here as the 6-Day data is not cumulative over that period
boosted_6_day_admits_no_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Six_Day_Admissions, data=clalit_data)
boosted_6_day_admits_no_adjustment %>%
  summary()

boosted_Log_C19_Death_Rate_6_day_admits_no_adjustment = lm(formula=Log_Boosted_COVID_Death_Rate ~ Six_Day_Admissions, data=clalit_data)
boosted_Log_C19_Death_Rate_6_day_admits_no_adjustment %>%
  summary()

boosted_6_day_admits_no_adjustment = lm(formula=Boosted_COVID_Deaths_by_Date ~ Six_Day_Admissions, data=clalit_data)
boosted_6_day_admits_no_adjustment %>%
  summary()



# Univariate Unboosted: Incremental and Cumulative COVID deaths as a function of 6-Day rolling average weekly admissions
# Cumulative less meaningful here as the 6-Day data is not cumulative over that period
unboosted_6_day_admits_no_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Six_Day_Admissions, data=clalit_data)
unboosted_6_day_admits_no_adjustment %>%
  summary()

unboosted_log_C19_Death_Rate_6_day_admits_no_adjustment = lm(formula=Log_Unboosted_COVID_Death_Rate ~ Six_Day_Admissions, data=clalit_data)
unboosted_log_C19_Death_Rate_6_day_admits_no_adjustment %>%
  summary()

unboosted_6_day_admits_no_adjustment = lm(formula=Unboosted_COVID_Deaths_by_Date ~ Six_Day_Admissions, data=clalit_data)
unboosted_6_day_admits_no_adjustment %>%
  summary()



# Univariate Boosted: Incremental COVID deaths or Cumulative COVID deaths as a function of 6-Day deaths
# Cumulative less meaningful here as the 6-Day death data is not cumulative over that period
boosted_6_day_deaths_no_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Six_Day_Deaths, data=clalit_data)
boosted_6_day_deaths_no_adjustment %>%
  summary()

boosted_Log_C19_Death_Rate_6_day_deaths_no_adjustment = lm(formula=Log_Boosted_COVID_Death_Rate ~ Six_Day_Deaths, data=clalit_data)
boosted_Log_C19_Death_Rate_6_day_deaths_no_adjustment %>%
  summary()

boosted_6_day_deaths_no_adjustment = lm(formula=Boosted_COVID_Deaths_by_Date ~ Six_Day_Deaths, data=clalit_data)
boosted_6_day_deaths_no_adjustment %>%
  summary()



# Univariate Unboosted: Incremental COVID deaths or Cumulative COVID deaths as a function of 6-Day deaths
# Cumulative less meaningful here as the 6-Day death data is not cumulative over that period
unboosted_6_day_deaths_no_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Six_Day_Deaths, data=clalit_data)
unboosted_6_day_deaths_no_adjustment %>%
  summary()

unboosted_Log_C19_Death_Rate_6_day_deaths_no_adjustment = lm(formula=Log_Unboosted_COVID_Death_Rate ~ Six_Day_Deaths, data=clalit_data)
unboosted_Log_C19_Death_Rate_6_day_deaths_no_adjustment %>%
  summary()

unboosted_6_day_deaths_no_adjustment = lm(formula=Unboosted_COVID_Deaths_by_Date ~ Six_Day_Deaths, data=clalit_data)
unboosted_6_day_deaths_no_adjustment %>%
  summary()



# Univariate Boosted: Incremental and Cumulative COVID deaths as a function of 6-Day cases
# Cumulative less meaningful here as the 6-Day case data is not cumulative over that period
boosted_6_day_cases_no_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Six_Day_Cases, data=clalit_data)
boosted_6_day_cases_no_adjustment %>%
  summary()

boosted_Log_C19_Death_Rate_6_day_cases_no_adjustment = lm(formula=Log_Boosted_COVID_Death_Rate ~ Six_Day_Cases, data=clalit_data)
boosted_Log_C19_Death_Rate_6_day_cases_no_adjustment %>%
  summary()

boosted_6_day_cases_no_adjustment = lm(formula=Boosted_COVID_Deaths_by_Date ~ Six_Day_Cases, data=clalit_data)
boosted_6_day_cases_no_adjustment %>%
  summary()



# Univariate Unboosted: Incremental and Cumulative COVID deaths as a function of 6-Day cases
# Cumulative less meaningful here as the 6-Day case data is not cumulative over that period
unboosted_6_day_cases_no_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Six_Day_Cases, data=clalit_data)
unboosted_6_day_cases_no_adjustment %>%
  summary()

unboosted_Log_C19_Death_Rate_6_day_cases_no_adjustment = lm(formula=Log_Unboosted_COVID_Death_Rate ~ Six_Day_Cases, data=clalit_data)
unboosted_Log_C19_Death_Rate_6_day_cases_no_adjustment %>%
  summary()

unboosted_6_day_cases_no_adjustment = lm(formula=Unboosted_COVID_Deaths_by_Date ~ Six_Day_Cases, data=clalit_data)
unboosted_6_day_cases_no_adjustment %>%
  summary()


#TODO: VERIFY THAT INTERCEPT ESTIMATE IS BEST FOR INTERACTION ROW OR FIRST ROW AS OF 9-8-2023 14:23

###########################################################################################################
# Now run multivariate using 6 day hospitalizations and person years
###########################################################################################################

############# BOOSTED ##############

# Multivariate Boosted: Incremental COVID deaths as a function of person years and 6-Day hospitalizations
boosted_py_hosp_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Boosted_Person_Years + Six_Day_Hospitalizations, data=clalit_data)
boosted_py_hosp_adjustment %>%
  summary()

boosted_log_C19_death_rate_py_hosp_adjustment = lm(formula=Log_Boosted_COVID_Death_Rate ~ Boosted_Person_Years + Six_Day_Hospitalizations, data=clalit_data)
boosted_log_C19_death_rate_py_hosp_adjustment %>%
  summary()

# NOW WITH INTERACTION **
boosted_py_hosp_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Boosted_Person_Years * Six_Day_Hospitalizations, data=clalit_data)
boosted_py_hosp_adjustment %>%
  summary()

boosted_log_C19_death_rate_py_hosp_adjustment = lm(formula=Log_Boosted_COVID_Death_Rate ~ Boosted_Person_Years * Six_Day_Hospitalizations, data=clalit_data)
boosted_log_C19_death_rate_py_hosp_adjustment %>%
  summary()



############# UNBOOSTED ##############

# Multivariate Unboosted: Incremental COVID deaths as a function of person years and 6-Day hospitalizations
unboosted_py_hosp_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Unboosted_Person_Years + Six_Day_Hospitalizations, data=clalit_data)
unboosted_py_hosp_adjustment %>%
  summary()

unboosted_log_C19_death_rate_py_hosp_adjustment = lm(formula=Log_Unboosted_COVID_Death_Rate ~ Unboosted_Person_Years + Six_Day_Hospitalizations, data=clalit_data)
unboosted_log_C19_death_rate_py_hosp_adjustment %>%
  summary()

# NOW WITH INTERACTION
unboosted_py_hosp_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Unboosted_Person_Years * Six_Day_Hospitalizations, data=clalit_data)
unboosted_py_hosp_adjustment %>%
  summary()

unboosted_log_C19_death_rate_py_hosp_adjustment = lm(formula=Log_Unboosted_COVID_Death_Rate ~ Unboosted_Person_Years * Six_Day_Hospitalizations, data=clalit_data)
unboosted_log_C19_death_rate_py_hosp_adjustment %>%
  summary()



###########################################################################################################
# Now run multivariate using 6 day weekly admits and person years
###########################################################################################################

############# BOOSTED ##############

# Multivariate Boosted: Incremental COVID deaths as a function of person years and 6-Day admits
boosted_py_hosp_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Boosted_Person_Years + Six_Day_Admissions, data=clalit_data)
boosted_py_hosp_adjustment %>%
  summary()

boosted_log_C19_death_rate_py_hosp_adjustment = lm(formula=Log_Boosted_COVID_Death_Rate ~ Boosted_Person_Years + Six_Day_Admissions, data=clalit_data)
boosted_log_C19_death_rate_py_hosp_adjustment %>%
  summary()

# Now with interaction
boosted_py_hosp_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Boosted_Person_Years * Six_Day_Admissions, data=clalit_data)
boosted_py_hosp_adjustment %>%
  summary()

boosted_py_hosp_adjustment = lm(formula=Log_Boosted_COVID_Death_Rate ~ Boosted_Person_Years * Six_Day_Admissions, data=clalit_data)
boosted_py_hosp_adjustment %>%
  summary()


############# UNBOOSTED ##############

# Multivariate Unboosted: Incremental COVID deaths as a function of person years and 6-Day admits
unboosted_py_hosp_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Unboosted_Person_Years + Six_Day_Admissions, data=clalit_data)
unboosted_py_hosp_adjustment %>%
  summary()

unboosted_log_C19_death_rate_py_hosp_adjustment = lm(formula=Log_Unboosted_COVID_Death_Rate ~ Unboosted_Person_Years + Six_Day_Admissions, data=clalit_data)
unboosted_log_C19_death_rate_py_hosp_adjustment %>%
  summary()

# Now with interaction
unboosted_py_hosp_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Unboosted_Person_Years * Six_Day_Admissions, data=clalit_data)
unboosted_py_hosp_adjustment %>%
  summary()

unboosted_py_hosp_adjustment = lm(formula=Log_Unboosted_COVID_Death_Rate ~ Unboosted_Person_Years * Six_Day_Admissions, data=clalit_data)
unboosted_py_hosp_adjustment %>%
  summary()



###########################################################################################################
# Now run multivariate using 6 day cases and person years
###########################################################################################################

############# BOOSTED ##############

# Multivariate Boosted: Incremental COVID deaths as a function of person years and 6-Day cases
boosted_py_hosp_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Boosted_Person_Years + Six_Day_Cases, data=clalit_data)
boosted_py_hosp_adjustment %>%
  summary()

boosted_log_C19_death_rate_py_hosp_adjustment = lm(formula=Log_Boosted_COVID_Death_Rate ~ Boosted_Person_Years + Six_Day_Cases, data=clalit_data)
boosted_log_C19_death_rate_py_hosp_adjustment %>%
  summary()

# Now with interaction
boosted_py_hosp_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Boosted_Person_Years * Six_Day_Cases, data=clalit_data)
boosted_py_hosp_adjustment %>%
  summary()

boosted_py_hosp_adjustment = lm(formula=Log_Boosted_COVID_Death_Rate ~ Boosted_Person_Years * Six_Day_Cases, data=clalit_data)
boosted_py_hosp_adjustment %>%
  summary()


############# UNBOOSTED ##############

# Multivariate Unboosted: Incremental COVID deaths as a function of person years and 6-Day cases
unboosted_py_hosp_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Unboosted_Person_Years + Six_Day_Cases, data=clalit_data)
unboosted_py_hosp_adjustment %>%
  summary()

unboosted_log_C19_death_rate_py_hosp_adjustment = lm(formula=Log_Unboosted_COVID_Death_Rate ~ Unboosted_Person_Years + Six_Day_Cases, data=clalit_data)
unboosted_log_C19_death_rate_py_hosp_adjustment %>%
  summary()

# Now with interaction
unboosted_py_hosp_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Unboosted_Person_Years * Six_Day_Cases, data=clalit_data)
unboosted_py_hosp_adjustment %>%
  summary()

unboosted_py_hosp_adjustment = lm(formula=Log_Unboosted_COVID_Death_Rate ~ Unboosted_Person_Years * Six_Day_Cases, data=clalit_data)
unboosted_py_hosp_adjustment %>%
  summary()




###########################################################################################################
###########################################################################################################
#########################################  PLACE HOLDER ANALYSIS  #########################################
# THE FOLLOWING ARE NOT USEFUL WITHOUT ACTUAL NON-COVID DEATH NUMBERS AS THESE HAD TO BE IMPUTED
# AT A CONSTANT RATE WHICH IS NOT LIKELY TO BE CORRECT. DATA NOT SHARED BY ARBEL ET AL..
# IN OUR CASE, THEY WERE IMPUTED AS A CONSTANT RATE AS A FRACTION OF THE PERSON-YEARS OF EXPOSURE.
# CONSEQUENTLY, WHICH WOULD OBVIOUSLY CREATE ARTIFACTUAL CORRELATIONS

# Multivariate Boosted: Incremental COVID deaths as a function of person years with additive effect of incremental Non-COVID deaths
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed and not provided
boosted_with_non_COVID_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Boosted_Person_Years + Boosted_Incremental_Non_COVID_Deaths, data=clalit_data)
boosted_with_non_COVID_adjustment %>%
  summary()

# Multivariate Unboosted: Incremental COVID deaths as a function of person years with additive effect of incremental Non-COVID deaths
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed and not provided
unboosted_with_non_COVID_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Unboosted_Person_Years + Unboosted_Incremental_Non_COVID_Deaths, data=clalit_data)
unboosted_with_non_COVID_adjustment %>%
  summary()

# Boosted: COVID deaths as a function of non-COVID deaths
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed and not provided
boosted_COVID_vs_Non_COVID = lm(formula=Boosted_Incremental_COVID_Deaths ~ Boosted_Incremental_Non_COVID_Deaths, data=clalit_data)
boosted_COVID_vs_Non_COVID %>%
  summary()

# Unboosted: COVID deaths as a function of non-COVID deaths
# # NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed and not provided
unboosted_COVID_vs_Non_COVID = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Unboosted_Incremental_Non_COVID_Deaths, data=clalit_data)
unboosted_COVID_vs_Non_COVID %>%
  summary()

# Boosted Univariate: COVID-to-Non-COVID death rate ratio for boosted
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed, and so the "rate" was imputed, and not provided
boosted_COVID_to_Non_COVID_Ratio = lm(formula=Boosted_Death_Rate_Ratios_COVID_to_Non_COVID ~ Boosted_Person_Years, data=clalit_data)
boosted_COVID_to_Non_COVID_Ratio %>%
  summary()

# Unboosted Univariate: COVID-to-Non-COVID death rate ratio for boosted
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed, and so the "rate" was imputed, and not provided
unboosted_COVID_to_Non_COVID_Ratio = lm(formula=Unboosted_Death_Rate_Ratios_COVID_to_Non_COVID ~ Unboosted_Person_Years, data=clalit_data)
unboosted_COVID_to_Non_COVID_Ratio %>%
  summary()

# Boosted Multivariate: COVID-to-Non-COVID death rate ratio for boosted with Non-COVID adjustment
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed, and so the "rate" was imputed, and not provided
boosted_COVID_to_Non_COVID_Ratio_with_Non_COVID = lm(formula=Boosted_Death_Rate_Ratios_COVID_to_Non_COVID ~ Boosted_Person_Years + Boosted_Incremental_Non_COVID_Deaths, data=clalit_data)
boosted_COVID_to_Non_COVID_Ratio_with_Non_COVID %>%
  summary()

# Unboosted Univariate: COVID-to-Non-COVID death rate ratio for unboosted with Non-COVID adjustment
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed, and so the "rate" was imputed, and not provided
unboosted_COVID_to_Non_COVID_Ratio_with_Non_COVID = lm(formula=Unboosted_Death_Rate_Ratios_COVID_to_Non_COVID ~ Unboosted_Person_Years + Unboosted_Incremental_Non_COVID_Deaths, data=clalit_data)
unboosted_COVID_to_Non_COVID_Ratio_with_Non_COVID %>%
  summary()

# Boosted Multivariate: COVID-to-Non-COVID death rate ratio for boosted with Non-COVID adjustment
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed, and so the "rate" was imputed, and not provided
boosted_COVID_to_Non_COVID_Ratio_with_COVID = lm(formula=Boosted_Death_Rate_Ratios_COVID_to_Non_COVID ~ Boosted_Person_Years + Boosted_Incremental_COVID_Deaths, data=clalit_data)
boosted_COVID_to_Non_COVID_Ratio_with_COVID %>%
  summary()

# Unboosted Univariate: COVID-to-Non-COVID death rate ratio for unboosted with Non-COVID adjustment
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed, and so the "rate" was imputed, and not provided
unboosted_COVID_to_Non_COVID_Ratio_with_COVID = lm(formula=Unboosted_Death_Rate_Ratios_COVID_to_Non_COVID ~ Unboosted_Person_Years + Unboosted_Incremental_COVID_Deaths, data=clalit_data)
unboosted_COVID_to_Non_COVID_Ratio_with_COVID %>%
  summary()


###########################################################################################################
########### BAR GRAPH PLOTS ON DEATHS, HEALTHY VACCINEE EFFECT ESTIMATE, AND HOSPITALIZATIONS #############
###########################################################################################################
###########################################################################################################

# VE Hospitalization bar graphs with error bars
bar_graph_data = read_delim('Bar_Graph_Hospitalization.csv', delim = ',', col_names = T, guess_max = 9000 )

describe(bar_graph_data)

head(bar_graph_data)
bar_colors=c('darkgray', 'lightblue', 'lightblue','darkgray','darkgray','darkgray')

plot <-ggplot(bar_graph_data) +
  labs(title = "Clalit Health published vaccine effectiveness against COVID-19 hospitalization",
       x = "Study",
       y = "Effectiveness") +
  geom_bar(aes(x=reorder(Study,Order), y=Effectiveness), stat="identity", fill=bar_colors, alpha=0.7) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) +
  geom_text(aes(x=Study, y=0.05, label = stringr::str_wrap(Doses, 10)), vjust = "inward", hjust = "leftward") +
  geom_errorbar( aes(x=Study, ymin=Effectiveness-ve_hosp_minus_error, ymax=Effectiveness+ve_hosp_plus_error), width=0.4, colour="orange", alpha=0.9, size=1.3)
plot + theme(plot.background = element_rect(fill = "lightgray"),
             panel.background = element_rect(fill = "lightgray", colour="lightgray"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.title = element_text(hjust = 0.5)) + ggtitle("Clalit Health published vaccine effectiveness against COVID-19 hospitalization")



# VE Death bar graphs with error bars in person-years analysis
bar_graph_data = read_delim('Bar_Graph_Death.csv', delim = ',', col_names = T, guess_max = 9000 )

describe(bar_graph_data)

head(bar_graph_data)
bar_colors=c('darkgray', 'lightyellow', 'lightyellow','darkgray', 'darkgray','darkgray','lightblue', 'lightblue')

plot <-ggplot(bar_graph_data) +
  labs(title = "Clalit Health published vaccine effectiveness against COVID-19 death",
       x = "Study",
       y = "Effectiveness") +
  geom_bar(aes(x=reorder(Study,Order), y=Effectiveness), stat="identity", fill=bar_colors, alpha=0.7) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) +
  geom_text(aes(x=Study, y=0.05, label = stringr::str_wrap(Doses, 10)), vjust = "inward", hjust = "leftward") +
  geom_errorbar(aes(x=Study, ymin=Effectiveness-ve_death_minus_error, ymax=Effectiveness+ve_death_plus_error), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  geom_hline(yintercept=.948, linetype="dashed", color = "darkgreen", size=1) +
  annotate("text", x=2.5, y=1.02, label=stringr::str_wrap("Non-Covid-19 Mortality Advantage in Boosted Unadjusted: 94.8%",30), size=4, color="darkgreen") +
  geom_segment(aes(x=1.6, y=1, xend=1.5, yend=0.95), arrow = arrow(length=unit(.3, 'cm')), color="darkgreen") +
  geom_bracket(
    aes(xmin = xmin, xmax = xmax, label = label),
    data = data.frame(xmin = c(2.2, 7.2), xmax = c(2.8, 7.8), label = c("Same Study", "Same Study")), y.position = c(0.35, 0.35)
  )

plot + theme(plot.background = element_rect(fill = "lightgray"),
             panel.background = element_rect(fill = "lightgray", colour="lightgray"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.title = element_text(hjust = 0.5)) + ggtitle("Clalit Health published vaccine effectiveness against COVID-19 death")

##############################################################################################
# VE deaths bar graph with error bars in deaths per 100,000 persons per day calculation
# Limited to the studies using deaths per 100,000 persons per day only which is in the
# file: Bar_Graph_Death_Persons_Per_Day_Only.csv

bar_graph_data = read_delim('Bar_Graph_Death_Persons_Per_Day_Only.csv', delim = ',', col_names = T, guess_max = 9000 )

head(bar_graph_data)
plot <-ggplot(bar_graph_data) +
  geom_bar( aes(x=Study, y=Effectiveness), stat="identity", fill="darkgray", alpha=0.7) +
  geom_text(aes(x=Study, y=0.05, label = stringr::str_wrap(Doses, 10)), vjust = "inward", hjust = "leftward") +
  geom_errorbar( aes(x=Study, ymin=Effectiveness-ve_death_minus_error, ymax=Effectiveness+ve_death_plus_error), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  geom_hline(yintercept=.948, linetype="dashed", color = "darkgreen", size=1) +
  annotate("text", x=1.5, y=.60, label=stringr::str_wrap("Unadjusted Non-COVID-19 mortality advantage: possible healthy vaccinee benefit (Arbel Dec-21)",50), size=4, color="darkgreen") +
  geom_segment(aes(x=1.5, y=.64, xend=1.5, yend=0.93), arrow = arrow(length=unit(.3, 'cm')), color="darkgreen") +
  geom_hline(yintercept=.946, linetype="dotted", color = "darkred", size=1) +
  annotate("text", x=3.5, y=.40, label=stringr::str_wrap("Unadjusted COVID-19 mortality advantage (Arbel Dec-21)",40), size=4, color="darkred") +
  geom_segment(aes(x=3.5, y=.44, xend=3.5, yend=0.93), arrow = arrow(length=unit(.3, 'cm')), color="darkred")

plot + theme(plot.background = element_rect(fill = "lightgray"),
             panel.background = element_rect(fill = "lightgray", colour="lightgray"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.title = element_text(hjust = 0.5)) + ggtitle(stringr::str_wrap("Clalit Health published vaccine effectiveness against COVID-19 death", 75))

##############################################################################################
##############################################################################################
##############################################################################################


# TIME SERIES LINE PLOTS FOR COVID DEATHS IN THE BOOSTED AND UNBOOSTED POPULATIONS
# OWID 6-day smoothed cases, weekly-hospitalizations, and deaths in Israel also plotted
# for comparison


plot <- ggplot(data=clalit_data, aes(x=Days)) +
        geom_line(aes(y = Unboosted_Incremental_COVID_Deaths), colour ="darkred") +
        geom_line(aes(y = Boosted_Incremental_COVID_Deaths), colour="steelblue") +
        annotate("text", x=10, y=25, label=stringr::str_wrap("Unboosted",50), size=4, color="darkred")  +
        annotate("text", x=20, y=5, label=stringr::str_wrap("Boosted",50), size=4, color="steelblue")  +
        labs(x='Day of Study', y='Incremental COVID-19 Deaths')

plot + theme(plot.background = element_rect(fill = "lightgray"),
             panel.background = element_rect(fill = "lightgray", colour="lightgray"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.title = element_text(hjust = 0.5)) +
             ggtitle(stringr::str_wrap("COVID-19 deaths over study period: Arbel et al. NEJM December 2021",45))

plot <- ggplot(data=clalit_data, aes(x=Days)) +
        geom_line(aes(y = Unboosted_COVID_Death_Rate), colour = "darkred") +
        geom_line(aes(y = Boosted_COVID_Death_Rate), colour="steelblue") +
        annotate("text", x=16, y=0.0075, label=stringr::str_wrap("Unboosted",50), size=4, color="darkred")  +
        annotate("text", x=34, y=0.003, label=stringr::str_wrap("Boosted",50), size=4, color="steelblue")  +
        labs(x='Day of Study', y='Incremental COVID-19 Death Rate')

plot + theme(plot.background = element_rect(fill = "lightgray"),
             panel.background = element_rect(fill = "lightgray", colour="lightgray"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.title = element_text(hjust = 0.5)) +
             ggtitle(stringr::str_wrap("COVID-19 death rate over study period: Arbel et al. NEJM December 2021", 45))

plot <- ggplot(data=clalit_data, aes(x=Days)) +
        geom_line(aes(y = Log_Unboosted_COVID_Death_Rate), colour = "darkred") +
        geom_line(aes(y = Log_Boosted_COVID_Death_Rate), colour="steelblue") +
        annotate("text", x=50, y=-2.25, label=stringr::str_wrap("Unboosted",50), size=4, color="darkred")  +
        annotate("text", x=30, y=-3.3, label=stringr::str_wrap("Boosted",50), size=4, color="steelblue")  +
        labs(x='Day of Study', y='Log(Incremental COVID-19 Death Rate')

plot + theme(plot.background = element_rect(fill = "lightgray"),
             panel.background = element_rect(fill = "lightgray", colour="lightgray"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.title = element_text(hjust = 0.5)) +
             ggtitle(stringr::str_wrap("COVID-19 Log(death rate) over study period; Arbel et al. NEJM December 2021", 45))
#symbols(x=c(0), y=c(0), circles=c(0.1), add=T, inches=F, fg=c('steelblue'))


plot <- ggplot(data=clalit_data, aes(x=Days)) +
  geom_line(aes(y = Unboosted_Fraction_of_Total_COVID_Deaths), colour ="darkred") +
  geom_line(aes(y = Boosted_Fraction_of_Total_COVID_Deaths), colour="steelblue") +
  annotate("text", x=10, y=0.15, label=stringr::str_wrap("Unboosted",50), size=4, color="darkred")  +
  annotate("text", x=17, y=0.05, label=stringr::str_wrap("Boosted",50), size=4, color="steelblue")  +
  labs(x='Day of Study', y='Incremental Fraction of Total COVID-19 Deaths')


plot + theme(plot.background = element_rect(fill = "lightgray"),
             panel.background = element_rect(fill = "lightgray", colour="lightgray"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.title = element_text(hjust = 0.5)) +
  ggtitle(stringr::str_wrap("Fraction of Total COVID-19 deaths over study period: Arbel et al. NEJM December 2021",45))


# WANT 2 Y-AXES FOR MEASURES ON DIFFERENT SCALES. THE 2 AXIS PLOTS WILL BE WITH 6-DAY AVERAGE
# CASES, HOSPITALIZATIONS, ADMISSIONS, AND DEATHS

# Value used to transform the data for cases
cases_coeff <- 1000000
admissions_coeff <- 100000
hospitalizations_coeff <- 100000
deaths_coeff <- 2000

# A few constants
unboosted_color <- "darkred"
boosted_color <- "steelblue"
epidemic_data_color <- "darkgreen"


#### 6-DAY AVERAGE CASES
ggplot(clalit_data, aes(x=Days)) +
  geom_line(aes(y=Six_Day_Cases), size=1, linetype="dashed", color=epidemic_data_color) +
  geom_line(aes(y=Unboosted_COVID_Death_Rate*cases_coeff), size=1, color=unboosted_color) +
  geom_line(aes(y=Boosted_COVID_Death_Rate*cases_coeff), size=1, color=boosted_color) +
  annotate("text", x=10, y=8000, label=stringr::str_wrap("6-Day Average Cases",50), size=4, color=epidemic_data_color)  +
  annotate("text", x=22, y=3000, label=stringr::str_wrap("Unboosted",50), size=4, color=unboosted_color) +
  annotate("text", x=20, y=500, label=stringr::str_wrap("Boosted",50), size=4, color=boosted_color)  +

  scale_y_continuous(

    # Features of the first axis
    name = "6-day Average COVID-19 Cases",

    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*cases_coeff, name="COVID-19 Death Rate (scaled for plot)")
  ) +

  theme(
    axis.title.y = element_text(color = epidemic_data_color, size=13),
    axis.title.y.right = element_text(color = "black", size=13)
  ) +

  ggtitle(stringr::str_wrap("COVID-19 Death Rates by Cohort (Arbel et al. NEJM December 2021) and Israeli 6-Day Average COVID-19 Cases",66))


#### 6-DAY AVERAGE HOSPITALIZATIONS
ggplot(clalit_data, aes(x=Days)) +
  geom_line(aes(y=Six_Day_Hospitalizations), size=1, linetype="dashed", color=epidemic_data_color) +
  geom_line(aes(y=Unboosted_COVID_Death_Rate*hospitalizations_coeff), size=1, color=unboosted_color) +
  geom_line(aes(y=Boosted_COVID_Death_Rate*hospitalizations_coeff), size=1, color=boosted_color) +
  annotate("text", x=40, y=1200, label=stringr::str_wrap("6-Day Average Hospitalizations",50), size=4, color=epidemic_data_color)  +
  annotate("text", x=22, y=900, label=stringr::str_wrap("Unboosted",50), size=4, color=unboosted_color) +
  annotate("text", x=20, y=50, label=stringr::str_wrap("Boosted",50), size=4, color=boosted_color)  +

  scale_y_continuous(

    # Features of the first axis
    name = "6-day Average COVID-19 Hospitalizations",

    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*hospitalizations_coeff, name="COVID-19 Death Rate (scaled for plot)")
  ) +

  theme(
    axis.title.y = element_text(color = epidemic_data_color, size=13),
    axis.title.y.right = element_text(color = "black", size=13)
  ) +

  ggtitle(stringr::str_wrap("COVID-19 Death Rates by Cohort (Arbel et al. NEJM December 2021) and Israeli 6-Day Average COVID-19 Hospitalizations",66))


#### 6-DAY AVERAGE WEEKLY ADMISSIONS
ggplot(clalit_data, aes(x=Days)) +
  geom_line(aes(y=Six_Day_Admissions), size=1, linetype="dashed", color=epidemic_data_color) +
  geom_line(aes(y=Unboosted_COVID_Death_Rate*admissions_coeff), size=1, color=unboosted_color) +
  geom_line(aes(y=Boosted_COVID_Death_Rate*admissions_coeff), size=1, color=boosted_color) +
  annotate("text", x=30, y=1200, label=stringr::str_wrap("6-Day Average Weekly Admissions",50), size=4, color=epidemic_data_color)  +
  annotate("text", x=22, y=900, label=stringr::str_wrap("Unboosted",50), size=4, color=unboosted_color) +
  annotate("text", x=20, y=50, label=stringr::str_wrap("Boosted",50), size=4, color=boosted_color)  +

  scale_y_continuous(

    # Features of the first axis
    name = "6-day Average COVID-19 Weekly Admissions",

    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*admissions_coeff, name="COVID-19 Death Rate (scaled for plot)")
  ) +

  theme(
    axis.title.y = element_text(color = epidemic_data_color, size=13),
    axis.title.y.right = element_text(color = "black", size=13)
  ) +

  ggtitle(stringr::str_wrap("COVID-19 Death Rates by Cohort (Arbel et al. NEJM December 2021) and 6-Day Average Weekly COVID-19 Admissions",66))


### 6-DAY AVERAGE DEATHS
ggplot(clalit_data, aes(x=Days)) +
  geom_line(aes(y=Six_Day_Deaths), size=1, linetype="dashed", color=epidemic_data_color) +
  geom_line(aes(y=Unboosted_COVID_Death_Rate*deaths_coeff), size=1, color=unboosted_color) +
  geom_line(aes(y=Boosted_COVID_Death_Rate*deaths_coeff), size=1, color=boosted_color) +
  annotate("text", x=7, y=25, label=stringr::str_wrap("6-Day Average Deaths",50), size=4, color=epidemic_data_color)  +
  annotate("text", x=25, y=9, label=stringr::str_wrap("Unboosted",50), size=4, color=unboosted_color) +
  annotate("text", x=20, y=.5, label=stringr::str_wrap("Boosted",50), size=4, color=boosted_color)  +

  scale_y_continuous(

    # Features of the first axis
    name = "6-day Average COVID-19 Deaths",

    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*deaths_coeff, name="COVID-19 Death Rate (scaled for plot)")
  ) +

  theme(
    axis.title.y = element_text(color = epidemic_data_color, size=13),
    axis.title.y.right = element_text(color = "black", size=13)
  ) +

  ggtitle(stringr::str_wrap("COVID-19 Death Rates by Cohort (Arbel et al. NEJM December 2021) and 6-Day Average COVID-19 Deaths", 66))

