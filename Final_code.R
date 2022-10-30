library(dplyr); library(lattice) ; library(nlme); library(lme4)

# Data : 8 variables #
# 1) the log-transformed average sale price in thousands of dollars (narsp)
# 2) average per capita income (ypc)
# 3) percentage growth in per capita income (perypc)
# 4) regulatory environment index in which high values mean more regulations (regtest)
# 5) rent control in which 0 means “No” and 1 means “Yes” (rcdum)
# 6) adjacent to a coastline in which 0 means “No” and 1 means “Yes” (ajwtr)
# 7) indicator for MSA (msa)
# 8) year 1 = 1986 to 9 = 1994 (time)

data <- read.table("hprice.csv", sep=",", header = TRUE)
head(data)
dim(data)
unique(data$msa)

data$msa = as.factor(data$msa)
summary(data)

# 1. Make a plot of the data on a single panel to show how housing prices increase by year. 
# Describe what can be seen in the plot.
xyplot(narsp ~ time, data=data, type=c("p","l"), group=msa,
       xlab="Years", col= "gray20",
       ylab="Housing_Prices(log)", main = "housing prices increase by year")

# 2. Fit a linear model with the (log) house price as the response and all other variables (except MSA) as fixed effect predictors. 
# Which terms are statistically significant? Discuss the coefficient for time.
fixed_lm <- lm(narsp ~ ypc+perypc+regtest+rcdum+ajwtr+time, data=data)
summary(fixed_lm)

# 3. Make a plot that shows how per-capita income changes over time. What is the nature of the increase? 
# Make a similar plot to show how income growth changes over time. Comment on the plot.
xyplot(ypc ~ time, data=data, type=c("p","l"), group=msa,
       xlab="Years", col= "gray20",
       ylab="per-capita income", main = "per-capita income changes over time")
xyplot(perypc ~ time, data=data, type=c("p","l"), group=msa,
       xlab="Years", col= "gray20",
       ylab="income_growth", main = "income growth changes over time")

# 4. Create a new variable that is the per-capita income for the first time period for each MSA. 
# Refit the same linear model but now using the initial income and not the income as it changes over time. 
# Compare the two models.
first_per = data[data$time==1, c('ypc', 'msa')]
dim(first_per)
colnames(first_per) = c("ypc_first","msa")
data = merge(data, first_per, by='msa')
fixed_lm2 <- lm(narsp ~ ypc_first+perypc+regtest+rcdum+ajwtr+time, data=data)
summary(fixed_lm)
summary(fixed_lm2)

# 5. Fit a general linear mixed effects model that has a random intercept for each MSA. Why might this be reasonable? 
# The rest of the model should have the same structure as in the previous question. 
# Make a numerical interpretation of the coefficient of time in your model.
lme1 <- lme(narsp ~ ypc_first+perypc+regtest+rcdum+ajwtr+time,
            data = data, random = ~ 1|msa , method = "ML")
summary(lme1)

# 6. Make the following diagnostic plots and interpret
# (i) Residuals vs. Fitted plot
plot(lme1, resid(., type="p") ~ fitted(.), id = 0.05, 
     xlab="Fitted", ylab="Residuals", main="Residuals vs. Fitted values")
# (ii) QQ plot of the residuals, 
qqnorm(lme1, ~ resid(.), id = 0.05, main="QQ plot of the Residuals")
# (iii) QQ plot of the random effects.
qqnorm(lme1, ~ ranef(.), id = 0.05, main="QQ plot of the Random Effects")

# 7. Fit a model that omits the adjacent to water and rent control predictors. 
# Test whether this reduction in the model can be supported.
lme2 <- lme(narsp ~ ypc_first+perypc+regtest+time,
            data = data, random = ~ 1|msa , method = "ML")
summary(lme2)
anova(lme1, lme2)

# 8. It is possible that the increase in prices may not be linear in year. 
# Fit a model where year is treated as a factor rather than a linear term. 
# Is this a better model than the previous choice? 
# Make a plot of the coefficients of the time factor that shows how prices have increased over time.
data$f_time = as.factor(data$time)
lme3 <- lme(narsp ~ ypc_first+perypc+regtest+rcdum+ajwtr+f_time,
            data = data, random = ~ 1|msa , method = "ML")
summary(lme3)
anova(lme1, lme3)
f_time_coef = lme3$coefficients$fixed[c(7:14)]
plot(f_time_coef~c(2:9), 
     xlab="time(year)", ylab="coefficients", main="Coefficients of the Time factor")

# 9. Interpret the coefficients in the previous model for the initial annual income, growth and regulation predictors.
summary(lme3)

# Discussion #
qqnorm(lme3, ~ resid(.), id = 0.05, main="QQ plot of the Residuals")


########## END ##########