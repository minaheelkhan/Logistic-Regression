---
title: "logistic regression"
output: html_document
date: "2025-05-10"

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

# loading packages
library(tidyverse)
library(car)
library(arm)
library(gridExtra)
library(dplyr)

```

## R Markdown

```{r}
raw.data <- read.csv("/Desktop/gambling2.csv", header = TRUE)
```

## Inspecting the data initially
```{r}
str(raw.data)
head(raw.data)
colnames(raw.data)
```


```{r}
table(raw.data$PROBGAM)

# keeping only the raws with values for probgam
data <- raw.data %>%
  filter(PROBGAM %in% c(0, 1))

data[data < 0] <- NA
```
```{r}
table(data$PROBGAM)
```

```{r}
# the initial socio-economic predictors
socioecon_vars <- c("EducEnd", "numcars", "HHSize", "eqv5", "HighQual", "Econact_2", "hhdtypb", "OwnRnt08", "hpnssec5", "SrcInc7", "SrcInc15", "eqvinc")

# chosen confounder variables
confounder_vars <- c("Sex", "age", "ethnicC", "Religsc")

# converting to factors
data <- data %>%
  mutate(eqv5 = factor(eqv5)) %>%
  mutate(HighQual = factor(HighQual)) %>%
  mutate(Econact_2 = factor(Econact_2)) %>%
  mutate(hhdtypb = factor(hhdtypb)) %>%
  mutate(OwnRnt08 = factor(OwnRnt08)) %>%
  mutate(hpnssec5 = factor(hpnssec5)) %>%
  mutate(SrcInc7 = factor(SrcInc7)) %>%
  mutate(SrcInc15 = factor(SrcInc15)) %>%
  mutate(sex = factor(Sex)) %>%
  mutate(ethnicC = factor(ethnicC)) %>%
  mutate(Religsc = factor(Religsc)) %>%
  mutate(numcars = factor(numcars)) %>%
  mutate(HHSize = factor(HHSize)) %>%
  mutate(totinc = factor(totinc))

summary(raw.data)
```

# GRAPHS
```{r}
# boxplots of the two continous variables
p1 <- ggplot(data, aes(x = factor(PROBGAM), y = eqvinc)) + 
  geom_boxplot() + coord_flip() + 
  theme_bw() + theme(legend.position = "none") + scale_fill_grey()

p2 <- ggplot(data, aes(x = factor(PROBGAM), y = age)) + 
  geom_boxplot() + coord_flip() + 
  theme_bw() + theme(legend.position = "none") + scale_fill_grey()

# histogram of age
p3 <- ggplot(data, aes(x = age)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Histogram of Age", x = "Age", y = "Count") +
  theme_minimal()

grid.arrange(p1, p2, p3, nrow = 1)
```


```{r}
# mosaic plots of categorical predictors
# seperated into 2 windows as they wouldn't all fit in one

par(mfrow = c(3, 3))

with(data, mosaicplot(table(OwnRnt08, PROBGAM), main = "Housing Tenure"))
with(data, mosaicplot(table(Econact_2, PROBGAM), main = "Employment Status"))
with(data, mosaicplot(table(eqv5, PROBGAM), main = "Income Quintile"))
with(data, mosaicplot(table(hhdtypb, PROBGAM), main = "Household Type"))
with(data, mosaicplot(table(HighQual, PROBGAM), main = "Qualification Level"))
with(data, mosaicplot(table(SrcInc7, PROBGAM), main = "Income Support"))
with(data, mosaicplot(table(SrcInc15, PROBGAM), main = "No Income Source"))
with(data, mosaicplot(table(hpnssec5, PROBGAM), main = "NS-SEC (5-category)"))
with(data, mosaicplot(table(numcars, PROBGAM), main = "Number of Cars"))

par(mfrow = c(3, 2))

with(data, mosaicplot(table(HHSize, PROBGAM), main = "Household Size"))
with(data, mosaicplot(table(totinc, PROBGAM), main = "Income Band"))
with(data, mosaicplot(table(sex, PROBGAM), main = "Sex"))
with(data, mosaicplot(table(ethnicC, PROBGAM), main = "Ethnicity"))
with(data, mosaicplot(table(Religsc, PROBGAM), main = "Religion"))

```


# EDA


# EducEnd
```{r}
table(data$EducEnd)

# contingency table converted to show percent calculations
educ_table <- table(data$EducEnd, data$PROBGAM)
educ_prop <- prop.table(educ_table, margin = 1)
educ_percent <- round(educ_prop * 100, 1)

educ_percent

# grouping and relabelling
data$EducEnd_group <- factor(
  ifelse(data$EducEnd %in% c(1, 2, 3), "Under 14",
  ifelse(data$EducEnd %in% c(4, 5, 6, 7), "Under 18",
  ifelse(data$EducEnd == 8, "19 or over", NA)))
)

# relevelling
data$EducEnd_group <- relevel(data$EducEnd_group, ref = "Under 18")

# regression to check significance
glm_educ_group <- glm(PROBGAM ~ EducEnd_group, data = data, family = binomial(link = "logit"))
display(glm_educ_group)

anova(glm_educ_group, test = "Chisq")

```



# numcars
```{r}
table(data$numcards)

numcars_table <- table(data$numcars, data$PROBGAM)
numcars_prop <- prop.table(numcars_table, margin = 1)
numcars_percent <- round(numcars_prop * 100, 1)

numcars_percent

data$numcars_group <- factor(data$numcars, levels = c(1, 2, 3), labels = c("1 car", "2 cars", "3 or more"))

data$numcars_group <- relevel(data$numcars_group, ref = "1 car")

glm_numcars <- glm(PROBGAM ~ numcars_group, data = data, family = binomial(link = "logit"))
display(glm_numcars)

anova(glm_numcars, test = "Chisq")

```

#HHSize
```{r}
table(data$HHSize)

HHSize_table <- table(data$HHSize, data$PROBGAM)
HHSize_prop <- prop.table(HHSize_table, margin = 1)
HHSize_percent <- round(HHSize_prop * 100, 1)
HHSize_percent

data$HHSize_group <- factor(
  ifelse(data$HHSize %in% c(1, 2), "1–2",
  ifelse(data$HHSize %in% c(3, 4, 5), "3–5",
  ifelse(data$HHSize %in% c(6, 7, 8, 9, 10, 11), "6+", NA)))
)

data$HHSize_group <- relevel(data$HHSize_group, ref = "1–2")

glm_HHSize <- glm(PROBGAM ~ HHSize_group, data = data, family = binomial(link = "logit"))
display(glm_HHSize)

anova(glm_HHSize, test = "Chisq")
```


#eqv5
```{r}
table(data$eqv5)

eqv5_table <- table(data$eqv5, data$PROBGAM)
eqv5_prop <- prop.table(eqv5_table, margin = 1)
eqv5_percent <- round(eqv5_prop * 100, 1)

eqv5_percent

data$eqv5_group <- factor(
  ifelse(data$eqv5 %in% c(1, 2), "High Income",
  ifelse(data$eqv5 == 3, "Middle Income",
  ifelse(data$eqv5 %in% c(4, 5), "Low Income", NA)))
)

data$eqv5_group <- relevel(data$eqv5_group, ref = "Middle Income")

glm_eqv5_group <- glm(PROBGAM ~ eqv5_group, data = data, family = binomial(link = "logit"))
display(glm_eqv5_group)

anova(glm_eqv5_group, test = "Chisq")
```

#HighQual

```{r}
table(data$HighQual)

HighQual_table <- table(data$HighQual, data$PROBGAM)
HighQual_prop <- prop.table(HighQual_table, margin = 1)
HighQual_percent <- round(HighQual_prop * 100, 1)
HighQual_percent

data$HighQual_group <- factor(
  ifelse(data$HighQual %in% c(1, 2), "Higher Education",
  ifelse(data$HighQual %in% c(3, 4), "Secondary/A-Levels",
  ifelse(data$HighQual %in% c(5, 6), "Low/No Quals", NA)))
)

data$HighQual_group <- relevel(data$HighQual_group, ref = "Secondary/A-Levels")

glm_HighQual <- glm(PROBGAM ~ HighQual_group, data = data, family = binomial(link = "logit"))
display(glm_HighQual)

anova(glm_HighQual, test = "Chisq")
```










#Econact_2
```{r}
table(data$Econact_2)

econ_table <- table(data$Econact_2, data$PROBGAM)
econ_prop <- prop.table(econ_table, margin = 1)
econ_percent <- round(econ_prop * 100, 1)
econ_percent

data$Econact_2 <- factor(data$Econact_2,
                         levels = c(1, 2, 3, 4, 5),
                         labels = c("Employed/Training",
                                    "Education",
                                    "Retired",
                                    "Unemployed",
                                    "Other"))

data$Econact_2 <- relevel(data$Econact_2, ref = "Employed/Training")

glm_Econact <- glm(PROBGAM ~ Econact_2, data = data, family = binomial(link = "logit"))
display(glm_Econact)

anova(glm_Econact, test = "Chisq")
```









# hhdtypb
```{r}
table(data$hhdtypb)

hhd_table <- table(data$hhdtypb, data$PROBGAM)
hhd_prop <- prop.table(hhd_table, margin = 1)
hhd_percent <- round(hhd_prop * 100, 1)
hhd_percent

data$hhdtypb_group <- factor(
  ifelse(data$hhdtypb == 1, "Single Adult",
  ifelse(data$hhdtypb %in% c(2, 5), "Multiple Adults",
  ifelse(data$hhdtypb %in% c(3, 4), "Family Household",
  ifelse(data$hhdtypb %in% c(6, 7), "Senior Household", NA))))
)

data$hhdtypb_group <- relevel(data$hhdtypb_group, ref = "Family Household")

glm_hhdtypb_group <- glm(PROBGAM ~ hhdtypb_group, data = data, family = binomial(link = "logit"))
display(glm_hhdtypb_group)

anova(glm_hhdtypb_group, test = "Chisq")
```

#OwnRnt08
```{r}
table(data$OwnRnt08)

own_table <- table(data$OwnRnt08, data$PROBGAM)
own_prop <- prop.table(own_table, margin = 1)
own_percent <- round(own_prop * 100, 1)
own_percent

data$OwnRnt08 <- factor(data$OwnRnt08,
  levels = c(1, 2, 3, 4, 5),
  labels = c("Owner",
             "Shared Ownership",
             "Social Rent",
             "Private Rent",
             "Other"))

data$OwnRnt08 <- relevel(data$OwnRnt08, ref = "Owner")

glm_own <- glm(PROBGAM ~ OwnRnt08, data = data, family = binomial(link = "logit"))
display(glm_own)

anova(glm_own, test = "Chisq")
```


#hppnsec
```{r}
table(data$hpnssec5)

hpnssec_table <- table(data$hpnssec5, data$PROBGAM)
hpnssec_prop <- prop.table(hpnssec_table, margin = 1)
hpnssec_percent <- round(hpnssec_prop * 100, 1)
hpnssec_percent

data$hpnssec5 <- factor(data$hpnssec5,
  levels = c(1, 2, 3, 4, 5, 99),
  labels = c("Managerial/Professional",
             "Intermediate",
             "Self-Employed",
             "Technical/Supervisory",
             "Semi-Routine",
             "Other")
)

data$hpnssec5 <- relevel(data$hpnssec5, ref = "Managerial/Professional")

glm_hpnssec <- glm(PROBGAM ~ hpnssec5, data = data, family = binomial(link = "logit"))
display(glm_hpnssec)

anova(glm_hpnssec, test = "Chisq")

```

# SrcInc7
```{r}
table(data$SrcInc7)

src7_table <- table(data$SrcInc7, data$PROBGAM)
src7_prop <- prop.table(src7_table, margin = 1)
src7_percent <- round(src7_prop * 100, 1)
src7_percent

data$SrcInc7 <- factor(data$SrcInc7,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

data$SrcInc7 <- relevel(data$SrcInc7, ref = "No")

glm_SrcInc7 <- glm(PROBGAM ~ SrcInc7, data = data, family = binomial(link = "logit"))
display(glm_SrcInc7)

anova(glm_SrcInc7, test = "Chisq")
```

# SrcInc15
  # it only has two categories and second category only has a handful of data points so excluding it from anaylsis. 
```{r}
table(data$SrcInc15)
```


#totinc
  # initially considered including totinc which represents the total income band, however evinc and eqv5 are already similar.
```{r}
table(data$totinc)

totinc_table <- table(data$totinc, data$PROBGAM)
totinc_prop <- prop.table(totinc_table, margin = 1)
totinc_percent <- round(totinc_prop * 100, 1)
totinc_percent
```




# Sex
```{r}
table(data$Sex)

sex_table <- table(data$Sex, data$PROBGAM)
sex_prop <- prop.table(sex_table, margin = 1)
sex_percent <- round(sex_prop * 100, 1)
sex_percent

data$Sex <- factor(
  ifelse(data$Sex == 1, "Male",
  ifelse(data$Sex == 2, "Female", NA))
)

data$Sex <- relevel(data$Sex, ref = "Male")

glm_sex <- glm(PROBGAM ~ Sex, data = data, family = binomial(link = "logit"))
display(glm_sex)

anova(glm_sex, test = "Chisq")
```

# ethnicC
```{r}
table(data$ethnicC)

ethnic_table <- table(data$ethnicC, data$PROBGAM)
ethnic_prop <- prop.table(ethnic_table, margin = 1)
ethnic_percent <- round(ethnic_prop * 100, 1)
ethnic_percent

data$ethnicC_group <- factor(
  ifelse(data$ethnicC %in% c(1, 2), "White",
  ifelse(data$ethnicC == 3, "Black",
  ifelse(data$ethnicC == 4, "Asian",
  ifelse(data$ethnicC == 5, "Mixed",
  ifelse(data$ethnicC == 6, "Other", NA)))))
)

data$ethnicC_group <- relevel(data$ethnicC_group, ref = "White")

glm_ethnic_group <- glm(PROBGAM ~ ethnicC_group, data = data, family = binomial(link = "logit"))
display(glm_ethnic_group)

anova(glm_ethnic_group, test = "Chisq")
```

# Religsc
```{r}
table(data$Religsc)

relig_table <- table(data$Religsc, data$PROBGAM)
relig_prop <- prop.table(relig_table, margin = 1)
relig_percent <- round(relig_prop * 100, 1)
relig_percent

data$Religsc_group <- factor(
  ifelse(data$Religsc == 1, "No Religion",
  ifelse(data$Religsc == 2, "Catholic Christian",
  ifelse(data$Religsc == 3, "Non-Catholic Crhistian",
  ifelse(data$Religsc %in% 4:9, "Other Religion", NA))))
)

data$Religsc_group <- relevel(data$Religsc_group, ref = "Catholic Christian")

glm_relig <- glm(PROBGAM ~ Religsc_group, data = data, family = binomial(link = "logit"))
display(glm_relig)

anova(glm_relig, test = "Chisq")
```

#VIF
# All the adjusted values are under 5 so keeping everything
```{r}
cleaned_data <- data[, c("PROBGAM", "age", "Sex", "ethnicC_group", "Religsc_group",
                        "HHSize_group", "numcars_group", "eqvinc", "hhdtypb_group",
                        "eqv5_group", "HighQual_group", "Econact_2", "OwnRnt08",
                        "hpnssec5", "SrcInc7")]


vif_model <- lm(as.numeric(PROBGAM) ~ ., data = cleaned_data)
vif(vif_model)
```
# MODEL
# forward selection. After adding my chosen predictors of interest, I removed the least significant variable ethnicity, which I had initially added as a confounder variable Multiple socio-economic variables also had non-significant values but I chose to keep them. Next, I tested out all other variables I had initially foregone and checked for significance by gradually adding them to the final model.
```{r}
glm1 <- glm(PROBGAM ~ ., data = cleaned_data, family = binomial(link = "logit"))
display(glm1, detail = TRUE, digits = 3)

Anova(glm1)
```
```{r}
# dropping ethnicity but keeping the rest because they're in my set of interest even if they're not significant
glmtemp <- glm(PROBGAM ~ . - ethnicC_group, data = cleaned_data, family = binomial(link = "logit"))
Anova(glmtemp)
display(glmtemp, detail = TRUE, digits = 3)
```
```{r}
# removed ethnicity and added first test variable, drating
model_data <- cleaned_data[, !(names(cleaned_data) %in% "ethnicC_group")]
model_data$drating <- data$drating

glm_updated <- glm(PROBGAM ~ ., data = model_data, family = binomial(link = "logit"))
Anova(glm_updated)
```

```{r}
# adding wemwbs
model_data <- model_data %>%
  mutate(wemwbs = data$wemwbs)
glm_updated <- glm(PROBGAM ~ ., data = model_data, family = binomial(link = "logit"))
Anova(glm_updated)
```
```{r}
# adding cigst1
model_data <- model_data %>%
  mutate(cigst1 = data$cigst1)
glm_updated <- glm(PROBGAM ~ ., data = model_data, family = binomial(link = "logit"))
Anova(glm_updated)
```
```{r}
# adding country
model_data <- model_data %>%
  mutate(country = data$country)
glm_updated <- glm(PROBGAM ~ ., data = model_data, family = binomial(link = "logit"))
Anova(glm_updated)
```

# factoring the newly added variables
  # drating (total units of alchol a week) is a scale so leave as is
  # wemwbs (well-being scale) also continuous, so leave as is
  # cigst1, kept the four categories as is, relevelled to never smoked
```{r}
table(data$cigst1)
cigst_table <- table(data$cigst1, data$PROBGAM)
cigst_prop <- prop.table(cigst_table, margin = 1)
cigst_percent <- round(cigst_prop * 100, 1)
print(cigst_percent)

model_data$cigst1 <- factor(
  ifelse(model_data$cigst1 == 1, "Never smoked",
  ifelse(model_data$cigst1 == 2, "Occasional smoker",
  ifelse(model_data$cigst1 == 3, "Regular smoker",
  ifelse(model_data$cigst1 == 4, "Current smoker", NA))))
)

model_data$cigst1 <- relevel(model_data$cigst1, ref = "Never smoked")

glm_cigst <- glm(PROBGAM ~ cigst1, data = model_data, family = binomial(link = "logit"))

library(car)
Anova(glm_cigst)
```
  # country, kept the two categories, relevelled to England
```{r}
model_data$country <- factor(
  ifelse(model_data$country == 1, "England",
  ifelse(model_data$country == 2, "Scotland", NA)),
  levels = c("England", "Scotland")
)

model_data$country <- relevel(model_data$country, ref = "England")

glm_country <- glm(PROBGAM ~ country, data = model_data, family = binomial(link = "logit"))
display(glm_country)

Anova(glm_country)
```


```{r}
# cleaning model_data
vars <- names(model_data)
model_data_complete <- model_data[complete.cases(model_data[, vars]), ]

glmfinal <- glm(PROBGAM ~ ., data = model_data, family = binomial(link = "logit"))   
display(glmfinal, detail=T, digits=3)
```

```{r}
# null model
null.glm <- glm(PROBGAM ~ 1, data = model_data_complete, family = binomial(link = "logit"))
anova(null.glm, glmfinal, test = "Chisq")
```
```{r}
# function from project guide code
ct.op<-function(predicted,observed){ #arguments
#create the data frame  
df.op<-data.frame(predicted=predicted,observed=observed)
#create a table 
op.tab<-table(df.op)
#use the prop.table function to obtain the rows we need and stack them on top of each other with rbind
op.tab<-rbind(op.tab,c(round(prop.table(op.tab,2)[1,1],2),
                       round((prop.table(op.tab,2)[2,2]),2)))
#name the rows
rownames(op.tab)<-c("pred=0","pred=1","%corr")
#name the columns
colnames(op.tab)<-c("obs=0","obs=1")
#return the table
op.tab
}


# checking for initial model, as there is an error for inconsistent rows, changing the data to only include rows with all values (which isn't much)

rows1 <- as.numeric(names(glm1$fitted.values))
final_data1 <- data$PROBGAM[rows1]
rowsfinal <- as.numeric(names(glmfinal$fitted.values))
final_datafinal <- data$PROBGAM[rowsfinal]

pred.glm1 <- as.numeric(glm1$fitted.values>0.15)
ct.op(pred.glm1, final_data1)

pred.glmfinal <- as.numeric(glmfinal$fitted.values>0.15)
ct.op(pred.glmfinal, final_datafinal)

```


```{r}
summary(glmfinal)
```
# Calculating Average Predictive Comparisons

```{r}
mod_mat <- model.matrix(glmfinal)
betas <- coef(glmfinal)
```



# Age (20 to 50) = -0.06636827
```{r}
# checking from 20 to 50 
lo.hi <- c(20, 50) 

colnames(mod_mat)
col_age <- which(colnames(mod_mat) == "age")

mm_hi <- mod_mat
mm_hi[, col_age] <- rep(lo.hi[2], nrow(mod_mat))

mm_lo <- mod_mat
mm_lo[, col_age] <- rep(lo.hi[1], nrow(mod_mat))

delta_age <- with(model_data, (invlogit(mm_hi %*% betas) - invlogit(mm_lo %*% betas)))

mean_delta_age <- mean(delta_age)
print(mean_delta_age)
```
# Sex (Male to Female) =  -0.07600407
```{r}
lo.hi <- c(0, 1)

col_sex <- which(colnames(mod_mat) == "SexFemale")

mm_hi <- mod_mat
mm_hi[, col_sex] <- rep(lo.hi[2], nrow(mod_mat))

mm_lo <- mod_mat
mm_lo[, col_sex] <- rep(lo.hi[1], nrow(mod_mat))

delta_sex <- with(model_data, (invlogit(mm_hi %*% betas) - invlogit(mm_lo %*% betas)))
mean_delta_sex <- mean(delta_sex)
print(mean_delta_sex)
```
# Houshold Size group (1-2 to 3-5) = -0.008180366
                      (1-2 to 6+) = 0.0525688
```{r}

col_hhsize_3_5 <- which(colnames(mod_mat) == "HHSize_group3–5")
col_hhsize_6p  <- which(colnames(mod_mat) == "HHSize_group6+")

mm_baseline <- mod_mat
mm_hhsize_3_5 <- mod_mat
mm_hhsize_6p <- mod_mat

mm_hhsize_3_5[, col_hhsize_3_5] <- 1
mm_hhsize_3_5[, col_hhsize_6p] <- 0

mm_hhsize_6p[, col_hhsize_3_5] <- 0
mm_hhsize_6p[, col_hhsize_6p] <- 1

pred_baseline <- invlogit(mm_baseline %*% betas)
pred_hhsize_3_5 <- invlogit(mm_hhsize_3_5 %*% betas)
pred_hhsize_6p <- invlogit(mm_hhsize_6p %*% betas)

delta_3_5 <- pred_hhsize_3_5 - pred_baseline
delta_6p <- pred_hhsize_6p - pred_baseline

mean_delta_3_5 <- mean(delta_3_5)
mean_delta_6p <- mean(delta_6p)

print(mean_delta_3_5)
print(mean_delta_6p)


```



# eqvinc (20000 to 80000) = -0.01907012
```{r}
lo_hi_eqvinc <- c(20000, 80000)

col_eqvinc <- which(colnames(mod_mat) == "eqvinc")

mm_hi <- mod_mat
mm_hi[, col_eqvinc] <- rep(lo_hi_eqvinc[2], nrow(mod_mat))

mm_lo <- mod_mat
mm_lo[, col_eqvinc] <- rep(lo_hi_eqvinc[1], nrow(mod_mat))

pred_hi <- invlogit(mm_hi %*% betas)
pred_lo <- invlogit(mm_lo %*% betas)

delta_eqvinc <- pred_hi - pred_lo
mean_delta_eqvinc <- mean(delta_eqvinc)

print(mean_delta_eqvinc)
```
# HighQual_group (Low/No Qualification to Higher Education) = -0.01887061
```{r}

col_highqual_higher <- which(colnames(mod_mat) == "HighQual_groupHigher Education")
col_highqual_lowno <- which(colnames(mod_mat) == "HighQual_groupLow/No Quals")

mm_higher <- mod_mat
mm_lowno <- mod_mat

mm_higher[, col_highqual_higher] <- 1
mm_higher[, col_highqual_lowno] <- 0

mm_lowno[, col_highqual_higher] <- 0
mm_lowno[, col_highqual_lowno] <- 1

pred_higher <- invlogit(mm_higher %*% betas)
pred_lowno <- invlogit(mm_lowno %*% betas)

delta_highqual <- pred_higher - pred_lowno
mean_delta_highqual <- mean(delta_highqual)

print(mean_delta_highqual)

```

# Units of alcohol a week (0 to 50) = 0.03551692
```{r}
col_drating <- which(colnames(mod_mat) == "drating")

lo <- 0
hi <- 50

mm_lo <- mod_mat
mm_lo[, col_drating] <- lo

mm_hi <- mod_mat
mm_hi[, col_drating] <- hi

pred_lo <- invlogit(mm_lo %*% betas)
pred_hi <- invlogit(mm_hi %*% betas)

delta_drating <- pred_hi - pred_lo
mean_delta_drating <- mean(delta_drating)

print(mean_delta_drating)
```

# Mental Health Wellbeing Score (30 to 70) = -0.06481117
```{r}

col_wemwbs <- which(colnames(mod_mat) == "wemwbs")

lo <- 30
hi <- 70   

mm_lo <- mod_mat
mm_lo[, col_wemwbs] <- lo

mm_hi <- mod_mat
mm_hi[, col_wemwbs] <- hi

pred_lo <- invlogit(mm_lo %*% betas)
pred_hi <- invlogit(mm_hi %*% betas)

delta_wemwbs <- pred_hi - pred_lo

mean_delta_wemwbs <- mean(delta_wemwbs)

print(mean_delta_wemwbs)
```

# Smoking Status (Never smoked to Current smoker) = 0.03615152
```{r}
col_regular <- which(colnames(mod_mat) == "cigst1Current smoker")

mm_lo <- mod_mat
mm_lo[, col_regular] <- 0

mm_hi <- mod_mat
mm_hi[, col_regular] <- 1

pred_lo <- invlogit(mm_lo %*% betas)
pred_hi <- invlogit(mm_hi %*% betas)

delta_cigst1 <- pred_hi - pred_lo

mean_delta_cigst1 <- mean(delta_cigst1)
print(mean_delta_cigst1)

```
# Country (England to Scotland) = -0.02162515
```{r}
col_country <- which(colnames(mod_mat) == "countryScotland")

mm_lo <- mod_mat
mm_lo[, col_country] <- 0

mm_hi <- mod_mat
mm_hi[, col_country] <- 1

pred_lo <- invlogit(mm_lo %*% betas)
pred_hi <- invlogit(mm_hi %*% betas)

delta_country <- pred_hi - pred_lo

mean_delta_country <- mean(delta_country)

print(mean_delta_country)
```

# Calculating odds-ratio
```{r}
coefficients <- coef(glmfinal)

# calculating odds ratio by exponentiating
odds_ratios <- exp(coefficients)

print(round(odds_ratios, 3))

```





