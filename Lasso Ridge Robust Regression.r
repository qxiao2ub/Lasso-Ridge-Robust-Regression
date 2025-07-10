---
title: ""
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



--------------------------------------------------------------------------------------

## Problem 1 

Use the fat data(copy and paste the following command into R console:library(faraway);data(“fat”)), and
use the percentage of body fat, siri, as the response and the other variables, except brozek and density as potential predictors. Use 70% of the data for train data set and use remaining data (30% of data) for test
data set (use set.seed(1023)). (50 points)

```{r}
#read data
library(faraway)
data("fat")
a8q1.dat = fat
```

```{r}
#get training data set
#select 70% as training data set
set.seed(1023)
n=dim(a8q1.dat)[1]
IND=sample(c(1:n),round(n*.7))
a8q1train.dat=a8q1.dat[IND,]
a8q1test.dat=a8q1.dat[-c(IND),]
```


a-)Linear regression with all predictors (5 pts)\newline

```{r}
a8q1.reg=lm(siri~age+weight+height+adipos+free+neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist,data = a8q1train.dat)
summary(a8q1.reg)
```

\textcolor{blue}{The regression model taking siri as response and all other variables except brozek and density as predictors are shown above in the regression model summary.}

b-)Linear regression with variables selected using stepwise (both ways) selection criteria (5 pts)\newline

```{r}
## Stepwise both ways
library(olsrr)
stepwise_models <- ols_step_both_p(a8q1.reg,p_enter = 0.1,p_remove = 0.05,details = TRUE)
```

```{r}
stepwise_models
```


c-)Ridge regression (5 pts)\newline

```{r}
library(glmnet)
```

```{r}
#need to define X and Y matrix for glmnet
#take out Y and law indicator from data for X (all independent variable)
x <- model.matrix(siri~age+weight+height+adipos+free+neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist, a8q1train.dat)[,-c(1)]
y <- a8q1train.dat$siri
RidgeMod <- glmnet(x, y, alpha=0, nlambda=100,lambda.min.ratio=0.0001)
```

```{r}
#we will use cross validation to select the best lamda
CvRidgeMod <- cv.glmnet(x, y, alpha=0, nlambda=100,lambda.min.ratio=0.0001)
```

```{r}
par(mfrow=c(1,1))
plot(CvRidgeMod)
```

```{r}
best.lambda.ridge <- CvRidgeMod$lambda.min
best.lambda.ridge
```

```{r}
coefficients(RidgeMod,s=best.lambda.ridge)
```

\textcolor{blue}{So from the above Ridge regression model, we can see the intercept and coefficients for all predictors.}

d-)Lasso (5 pts)\newline

```{r}
LassoMod <- glmnet(x, y, alpha=1, nlambda=100,lambda.min.ratio=0.0001)
plot(LassoMod,xvar="norm",label=TRUE)
```

```{r}
CvLassoMod <- cv.glmnet(x, y, alpha=1, nlambda=100,lambda.min.ratio=0.0001)
```

```{r}
plot(CvLassoMod)
```

```{r}
best.lambda.lasso <- CvLassoMod$lambda.min
best.lambda.lasso
```

```{r}
coef(CvLassoMod, s = "lambda.min")
```

\textcolor{blue}{Above the intercept and coefficients are for the Lasso regression method and retrieved parameters.}


e-)Elastic Net (5 pts)\newline

```{r}
EnetMod <- glmnet(x, y, alpha=0.5, nlambda=100,lambda.min.ratio=0.0001)
CvElasticnetMod <- cv.glmnet(x, y,alpha=0.5,nlambda=100,lambda.min.ratio=0.0001)
```

```{r}
best.lambda.enet <- CvElasticnetMod$lambda.min
best.lambda.enet
```

```{r}
coefficients(EnetMod,s=best.lambda.enet)
```

\textcolor{blue}{Above is using the ElasticNet and retrieve all coefficients and intercept.}

f-)Robust Regression (5 pts)\newline

```{r}
library(MASS)
a8q1robust.reg=MASS::rlm(siri~age+weight+height+adipos+free+neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist, psi = psi.huber, data = a8q1train.dat)
summary(a8q1robust.reg)
```

\textcolor{blue}{Above is the coefficients of Robust regression coefficients.}


g-)Use the models you find to predict the response in the test sample. Make a report on the performances of the models. (20 pts)\newline

```{r}
f<-lm(siri~age+weight+height+adipos+free+neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist,data=a8q1test.dat)
y_hat.ridge <- predict(RidgeMod, s = best.lambda.ridge, newx = x)
y_hat.lasso <- predict(LassoMod, s = best.lambda.lasso, newx = x)
y_hat.enet <- predict(CvElasticnetMod , s = best.lambda.enet, newx = x)
y_hat.robust <- predict(a8q1robust.reg , s = best.lambda.enet, newx = x)
library(caret)
```

```{r}
Model.pred.ridge<-data.frame(obs = a8q1train.dat$siri, pred=c(y_hat.ridge))
Model.pred.lasso<-data.frame(obs = a8q1train.dat$siri, pred=c(y_hat.lasso))
Model.pred.enet<-data.frame(obs = a8q1train.dat$siri, pred=c(y_hat.enet))
Model.pred.robust<-data.frame(obs = a8q1train.dat$siri, pred=c(y_hat.robust))
defaultSummary(Model.pred.ridge)
```

```{r}
out=rbind(defaultSummary(Model.pred.ridge),defaultSummary(Model.pred.lasso),defaultSummary(Model.pred.enet))
dimnames(out)[[1]]<-c("Ridge","Lasso","Elastic Net")
out
```

\textcolor{blue}{The RMSE, R-square and MAE for all Ridge regression, Lasso regression, Elastic Net and Robust regression are shown above.}


## Problem 2

Use the fat data set in previous example by using the percentage of body fat, siri, as the response and the other variables, except brozek and density as potential predictors.(25 points)

a-)Fit the same model in question 1-a but now using Huber's robust method. Comment on any substantial differences between this model and the least squares fit. (10 points)

```{r}
# Huber Robust Regression
a8q2a.reg.huber <- MASS::rlm(siri~age+weight+height+adipos+free+neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist, psi = psi.huber, data = a8q1.dat)
summary(a8q2a.reg.huber)
```

```{r}
# Least Squares using full data
a8q2a.reg.full <- stats::lm(siri~age+weight+height+adipos+free+neck+chest+abdom+hip+thigh+knee+ankle+biceps+forearm+wrist, data = a8q1.dat)
summary(a8q2a.reg.full)
```



b-)Identify which two cases have the lowest weights in the Huber fit. What is unusual about these two points? (5 points)

```{r}
olsrr::ols_plot_resid_lev(a8q2a.reg.full)
```

```{r}
# Huber Weights
weights <- a8q2a.reg.full[["w"]]
```

\textcolor{blue}{The two lowest weights belong to observations of point 39 and 42.}

c-)Plot weight (of the man) against height. Identify the two outlying cases. Are these the same as those identified in the previous question? Discuss.(10 points)

```{r}
# Plot
ggplot2::ggplot(a8q1.dat, ggplot2::aes(x=height, y=weight)) +
ggplot2::geom_point() + # Show dots
ggplot2::geom_text(
label=rownames(a8q1.dat),
nudge_x = 0.25, nudge_y = 0.25,
check_overlap = T)
```

\textcolor{blue}{The outliers by the weight vs. height plot are 39 and 42.}

## Problem 3

Using the stackloss data (copy and paste the following command into R console:library(faraway);data(“stackloss”)), fit a model with stack.loss as the response and the other three variables as predictors using the following methods: (25 Points)

a-) Least squares (5 points)\newline

```{r}
library(faraway)
data("stackloss")
```

```{r}
stackloss
```

```{r}
# Least Squares using full data
a8q3a.reg.least <- stats::lm(stack.loss ~ ., data = stackloss)
summary(a8q3a.reg.least)
```


b-) Huber robust regression method (5 points)\newline

```{r}
# Huber's robust method
a8q3b.reg.huber=MASS::rlm(stack.loss ~ ., psi=psi.huber, data = stackloss)
summary(a8q3b.reg.huber)
```

c-) Bisquare robust regression method (5 points)\newline

```{r}
# Bisquare robust method
a8q3b.reg.bisquare=MASS::rlm(stack.loss ~ ., psi=psi.bisquare, data = stackloss)
summary(a8q3b.reg.bisquare)
```

\textcolor{blue}{Bisquare robust regression model is depicted in above summary figure.}

d-) Compare the results. Now use diagnostic methods to detect any outliers or influential points. Remove these points and then use least squares. Compare the results. (10 points)\newline

```{r}
#outlier for least square
plot(a8q3a.reg.least,which=3)
```

```{r}
n <- nrow(stackloss)
n
```

```{r}
#outlier for Huber's method
plot(a8q3b.reg.huber,which=3)
```

```{r}
#outlier for bisquare method
plot(a8q3b.reg.bisquare,which=3)
```

\textcolor{blue}{By Least square, Huber robust regression and Bisquare robust regression methods, we see outliers are point 3, 4 and 21.}

```{r}
# check influential points: use a visual for least square method
library(car)
influencePlot(a8q3a.reg.least, id="noteworthy", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")
```

```{r}
p <- length(a8q3a.reg.least$coefficients)
Bonferroni_Critical_Value<-abs(qt(.05/(n*p),n-p-1))
Bonferroni_Critical_Value
```

\textcolor{blue}{As we see point 4 and 21's Studres values<3.93, so we see point 4 and 21 are influential outliers.}

```{r}
#remove data 4 and 21
a8q3d.dat=stackloss[-c(4,21),]
a8q3d.dat
```

```{r}
# Least Squares using after removal influential and outliers data
a8q3d.reg.least <- stats::lm(stack.loss ~ ., data = a8q3d.dat)
summary(a8q3d.reg.least)
```

\textcolor{blue}{After removal the influential and outliers points, the R-square value is increased comparing to before. }








