# Springboard-Linear-Regression-Miniv2-Project
---
title: "Springboard Linear Regression Mini Project"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R
```{r,message=FALSE}
states.data<-readRDS("Desktop/states.rds")

states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
tail(states.info,8)
```
## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.
# summary of expense and csat columns, all rows
```{r}
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
```
# correlation between expense and csat
```{r}
cor(sts.ex.sat)
## Plot the data before fitting models
## ───────────────────────────────────────
```
##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat

plot(sts.ex.sat)
## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod<- lm(csat~expense,data=states.data)
#summarize and print the results
summary(sat.mod)
## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))
## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:
class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit
confint(sat.mod)
hist(residuals(sat.mod))

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4,4,2,2),mfrow=c(1,2))
plot(sat.mod,which = c(1,2))
## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors

sat.voting.mod <- lm(csat~expense+house+senate,data = na.omit(states.data))
sat.mod<-update(sat.mod,data=na.omit(states.data))
#compare using anova (analysis of variance)
anova(sat.mod,sat.voting.mod)
coef(summary(sat.voting.mod))
## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?


##examine/plot the data before fitting the model
sts.energy.metro<- subset(states.data,select = c("energy","metro"))
summary(sts.energy.metro)
cor(sts.energy.metro)
plot(sts.energy.metro)
#at first glance it looks like energy consumed isn't related to population density.  that is, we can have one with a low density or high density and energy consumed looks to be about the same.  
energy.mod<-lm(energy~metro,data=states.data)
##print the model summaryclass(energy.mod)


summary(energy.mod)
class(energy.mod)
names(energy.mod)
confint(energy.mod)
hist(residuals(energy.mod))

# add another variable and see if it improves the model...let's see what adding population (pop) does
energy.pop.mod<- lm(energy~metro+pop,data=na.omit(states.data))
energy.mod<-update(energy.mod,data=na.omit(states.data))
anova(energy.mod,energy.pop.mod)
coef(summary(energy.pop.mod))
summary(energy.pop.mod)
#interpreting the output...can you please review and let me now where i'm on and where i'm off with the interpretation below?
#define the pieces:
#call....shows the formula; note we're calling energy as a function of metro and population

# coefficient estimate: intercept is effectively the expected energy consumption given an average population and density from the data set....so for the average pop and metro, consumption would be 5.523e+02.  the rows underneath the intercept are the slope of the model, so for an additional increase in metro, consumption would go down -1.739e+00 and pop would cause it to go up 6.089e-07
# coefficient standard error: this measures the average amount the coefficient estimates vary from teh actual average value of our response variable.  standard error gives us an estimate of teh expected difference in case we ran the model repeatedly.  so, we can say the energy consumption can vary by 9.140e-01
# coefficient t value--this is a measure of how many standard deviations our coefficient estimate is far away from 0.  we'd like this to be far away from 0 so we can reject the null hypothesis...that is we could declare a relationship exists between metro and energy consumption. it looks like metro may have a negative correlation to energy (-1.903) and we likely can't reject null hypothesis for pop that is, there likely is not a relationship between pop and energy consumption
#Pr(>t)  this is the probability of observing any value equal or larger than t.  a small p value would indicate it's unlikely we'll observe a relationship between the predictor (metro) and response (energy) due to chance.  rule of thumb is a p value of 5% or less is a good cut off point.  our p value is close to 0 for metro and not for pop.  also, if we look at signif codes, metro has a '.' and pop has ' ' indicating not a strong p value for either of them
# residual standard error:  this measures the quality of the linear regression fit.  so for this model, we'd look at 113.5/4.523e+02 to get the percenrage errors (what any prediction would still be off by) so 113.5/452.3=25.09%.  this was calculated using 45 degrees of freedome....so 47 data points less two parameters (intercept and slope)
# multipel R squared provies a measure of how well the model fits the actual data.  it is a proportion of the linear relationship between our predictor variable (pop and metro) and our response/target variable (energy)  here we get .0977 or 9.77% so about 10% of the variance found in the response variable (energy) can be explained by the predictor variable (metro and pop) because more variables increase the r squared, using the adjusted r squared is the preferred measure as it adjusts for the number of variables considered
# f statistic---this is a good indicator of whether there is a relationship between our predictor and response variables.  the further the f statistic is from 1 the better it is.  our f statistic is only 2.436 so relatively not larger than 1
# let's throw everything in and take a look at what the model calls most significant:
kitchen.sink<-lm(energy~income+high+college+expense+percent+senate+house+green+toxic+miles+waste+metro+density+area+pop+region,data=na.omit(states.data))
summary(kitchen.sink)
#looks like green, toxic are significant and area may play a role..note removing most of the variables and keeping three improves our adj r squared from 73.33% to 76.52%.
energy.rev<-lm(energy~green+toxic+area,data=na.omit(states.data))
summary(energy.rev)
# let's drop area and see what it does
energy.rev.v2<-lm(energy~green+toxic,data=na.omit(states.data))
# this lowers our adjusted r squared so it to 75.21%
# so our energy.rev original model is the best explanation.  we can account for 76.52% of energy given the three variables of green, toxic and area


#I can input the above ok/follow the examples but I'd like to get better at being able to explain my results.  resources on interpreting linear models?
## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?
# add the interaction to the model
sat.expense.by.percent<-lm(csat~expense*income,data=states.data)
#show the results
coef(summary(sat.expense.by.percent))

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical

str(states.data$region)
states.data$region<- as.factor(states.data$region)
#add region to the model
sat.region<-lm(csat~region, data=states.data)
#show the results

coef(summary(sat.region))
anova(sat.region)
##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

 #print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

energy.rev.w.region<-lm(energy~green+toxic+area*region,data=na.omit(states.data))
summary(energy.rev.w.region)
anova(energy.rev.w.region)


