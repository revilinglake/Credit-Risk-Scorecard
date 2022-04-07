install.packages("xlsx")
install.packages("scorecard")
install.packages("Information")
install.packages("gmodels")
library("gmodels")
library("scorecard")
library("Information")
library("xlsx")
library("dplyr")

german <- read.xlsx("German.xlsx",1)
head(german)
nrow(german)
ncol(german)
summary(german)

# The 'Purpose" variable might require some cleaning.
# There are unrecognized 'X' characters.
# Referring to the data dictionary, this can be the last 'other' category that is supposed to be represented by '10'.
# Let us change all the 'X' to '10' and convert the variable into an numerical categorical variable.
# Purpose does not have any data for the '7'(vacation) category.

german["Purpose"][german["Purpose"] == "X"] <- "10"
as.numeric(unique(german$Purpose))
german$Purpose <- as.numeric(german$Purpose)
summary(german)

# The purpose variable is now usable in building models.

l12 <- german[german$Duration <= 12,]
m12 <- german[german$Duration > 12,]
summary(l12)
summary(m12)

# Throughout the code 'l12' refers to credit data that has Duration <= 12.
# Throughout the code 'm12' refers to credit data that has Duration > 12.

table(l12$Good)
table(m12$Good)

l12_traintest = split_df(l12, y = "Good", ratio = 0.79, seed = 7)
m12_traintest = split_df(m12, y = "Good", ratio = 0.65, seed = 7)
l12_train <- l12_traintest$train
l12_test <- l12_traintest$test
m12_train <- m12_traintest$train
m12_test <- m12_traintest$test

# 2a) Random Sampling based on good/bad ratio, using Good as target variable.
# 2b) Simple answer is to measure the performance of the models, for example using ROC.
# 2c) There were no specific issues, but it is good to note that for m12, the good to bad ratio was much less when compared to l12.

table(l12_train$Good)
table(m12_train$Good)
table(m12_test$Good)
table(l12_test$Good)

l12_info <- create_infotables(l12_train, NULL, y="Good")
m12_info <- create_infotables(m12_train, NULL, y="Good")
l12_info
m12_info

# One of the interesting points is that duration has more IV in l12 than it has in m12, even though m12 has significantly more Duration values.

l12_info$Summary
# Checking -> Categorical
# History -> Categorical
# Property -> Categorical
# Duration -> Continuous

# Lets check and compare the correlation of these top categorical variables selected based on the IV using Chi Sq Independence test with the target "Good" variable.
# Duration is the only continuous variable in the top 4.

with(l12_train, CrossTable(Good, Checking, digits = 1, prop.chisq = F, chisq = T))
with(l12_train, CrossTable(Good, History, digits = 1, prop.chisq = F, chisq = T))
with(l12_train, CrossTable(Good, Property, digits = 1, prop.chisq = F, chisq = T))

# All three variables are correlated to the target variable, since we reject null hypothesis(Variables are independent) in all three Chi Square tests.
# There is a warning that the History Chi Square value may be wrong.
# The warning we are getting when doing the Chi Sq tests is because there are too few data points in some of the categories.
# Hence, the test is not completely accurate. We can rely on a combination of observation of IV and the Chi Sq Test results in such cases.

with(l12_train, CrossTable(Good, Emploed, digits = 1, prop.chisq = F, chisq = T))
# Eploed has a weak correlation with the target variable when compared to Checking, History and Property.
# Hence our choice of categorical variables is final.
# Any IV below 0.2 is considered pretty weak according to a rule of thumb proposed by Siddiqui.

m12_info$Summary
# Checking -> Categorical
# Savings -> Categorical
# Purpose -> Categorical
# Age -> Continuous
# History -> Categorical

# According to the info table these 5 variables have the highest IV. Lets Chi Sq test the categorical variables.

with(m12_train, CrossTable(Good, Checking, digits = 1, prop.chisq = F, chisq = T))
# Chi Sq value for 'Checking' in m12 is much higher for checking than l12.
with(m12_train, CrossTable(Good, Savings, digits = 1, prop.chisq = F, chisq = T))
with(m12_train, CrossTable(Good, Purpose, digits = 1, prop.chisq = F, chisq = T))
with(m12_train, CrossTable(Good, History, digits = 1, prop.chisq = F, chisq = T))

# All 4 are related to the target varibale, but Checking, Savings and Purpose have a higher Chi Sq value than History.
# Hence, based on the IV and Chi Sq tests we select Checking, Savings, Purpose, and Age as the four variables.

# NOTE: For continuous variables, we use the IV values from the info table to measure the correlation with the target variable.
# The final selection for l12: Checking, History, Property, and Duration.
# The final selection for m12: Checking, Savings, Purpose, and Age.

# Let us create training subsets using the variables we selected.
l12_trainsubset <- l12_train[,c('Checking', 'History', 'Property', 'Duration', 'Good')]
m12_trainsubset <- m12_train[,c('Checking', 'Savings', 'Purpose', 'Age', 'Good')]

# We will check WoE to decide on how many bins for each continuous variables is going to be the best.

woebin(l12_train, y = "Good", method="chimerge", positive="bad|0")

# Binning for l12.

woebin(l12_trainsubset, y = "Good", method="chimerge", positive="bad|0")

# Checking what class division is best for Duration.
woebin(l12_trainsubset, y = "Good", method="chimerge", positive="bad|0", var_skip = c('Checking', 'History', 'Property'), breaks_list = list( Duration = c(9,11,12)))
woebin(l12_trainsubset, y = "Good", method="chimerge", positive="bad|0", var_skip = c('Checking', 'History', 'Property'), breaks_list = list( Duration = c(5,8,12)))
woebin(l12_trainsubset, y = "Good", method="chimerge", positive="bad|0", var_skip = c('Checking', 'History', 'Property'), breaks_list = list( Duration = c(5,9,12)))
woebin(l12_trainsubset, y = "Good", method="chimerge", positive="bad|0", var_skip = c('Checking', 'History', 'Property'), breaks_list = list( Duration = c(5,7,12)))
woebin(l12_trainsubset, y = "Good", method="chimerge", positive="bad|0", var_skip = c('Checking', 'History', 'Property'), breaks_list = list( Duration = c(4,7,12)))

# There is no Duration less than 5. Duration might do better if we lessen the bins.
woebin(l12_trainsubset, y = "Good", method="chimerge", positive="bad|0", breaks_list = list( Duration = c(7,12)))

# Checking has a good WoE monotonity, but we can try to combine some categories and achieve similar results.
# History has a weak monotonic relationship with the target. We can try to make that better by combining some categories.
# Property has similar problems to history.

# Trial and Error with Woebins untill a good WoE satisfied solution is obtained.
breaks_list = list(Checking = c(2, 3), History = c(3, 4), Property = c(3, 4),Duration = c(7, 12))
woebin(l12_trainsubset, y = "Good", method="chimerge", positive="bad|0", breaks_list = breaks_list)
breaks_list = list(Checking = c(3, 4), History = c(2, 3), Property = c(2, 3),Duration = c(7, 12))
woebin(l12_trainsubset, y = "Good", method="chimerge", positive="bad|0", breaks_list = breaks_list)$Checking
breaks_list = list(Checking = c(2, 4), History = c(2, 3, 4), Property = c(2, 4),Duration = c(7, 12))
woebin(l12_trainsubset, y = "Good", method="chimerge", positive="bad|0", breaks_list = breaks_list)
breaks_list = list(Checking = c(2, 3), History = c(2, 4), Property = c(2, 4),Duration = c(7, 12))
woebin(l12_trainsubset, y = "Good", method="chimerge", positive="bad|0", breaks_list = breaks_list)
l12_breaks_list = list(Checking = c(3, 4), History = c(2, 3), Property = c(3, 4),Duration = c(7, 12))
woebin(l12_trainsubset, y = "Good", method="chimerge", positive="bad|0", breaks_list = l12_breaks_list)

# Checking: Breaks at 3,4 are best according to WoE.Hence we are merging 1 and 2 categories of Checking. (Check meaning in real data)
# History: Breaks at 2,3 are best according to WoE.Hence we are merging 3 and 4 categories of History. (Check meaning in real data)
# Property: Breaks at 3,4 are best according to WoE.Hence we are merging 1 and 2 categories of Property. (Check meaning in real data)
# Duration: Best split at 7,12.

# Binning for m12.

woebin(m12_trainsubset, y = "Good", method="chimerge", positive="bad|0")
# Checking already has a very good WoE monotony as well as a good total IV, hence we are going to keep the 2,3,4 split.
# Savings has a skewed monotonic relationship. We can try to combine some categories to achieve better results.
# Purpose does not have monotony at all.
# Age does not have monotony as well. We will try different classes till we get better results.
       
# Trial and Error with Woebins untill a good WoE satisfied solution is obtained.
m12_breaks_list = list(Savings = c(3, 4), Purpose = c(2, 5, 7, 9), Age = c(22, 26, 30, 34, 38, 42))
woebin(m12_trainsubset, y = "Good", method="chimerge", positive="bad|0", breaks_list = m12_breaks_list)
m12_breaks_list = list(Savings = c(2, 3), Purpose = c(3, 5, 7),Age = c(25, 30, 34, 40, 50))
woebin(m12_trainsubset, y = "Good", method="chimerge", positive="bad|0", breaks_list = m12_breaks_list)
m12_breaks_list = list(Savings = c(2, 3), Purpose = c(4, 5, 7),Age = c(25, 30, 34, 45))
woebin(m12_trainsubset, y = "Good", method="chimerge", positive="bad|0", breaks_list = m12_breaks_list)
m12_breaks_list = list(Savings = c(3, 4), Purpose = c(4, 7),Age = c(25, 35, 40, 45))
woebin(m12_trainsubset, y = "Good", method="chimerge", positive="bad|0", breaks_list = m12_breaks_list)
m12_breaks_list = list(Savings = c(3, 4), Purpose = c(1, 3),Age = c(25, 35, 40))
woebin(m12_trainsubset, y = "Good", method="chimerge", positive="bad|0", breaks_list = m12_breaks_list)
m12_breaks_list = list(Savings = c(3, 4), Purpose = c(1, 3),Age = c(24, 30, 35))
woebin(m12_trainsubset, y = "Good", method="chimerge", positive="bad|0", breaks_list = m12_breaks_list)

# Checking: Breaks at 2,3,4 are best according to WoE.
# Savings: Breaks at 3,4 are best according to WoE.Hence we are merging (1,2), and (4,5) categories of Savings. (Check meaning in real data)
# Purpose: Breaks at 1,3 are best according to WoE.Hence we are merging (1,2), and (3...9) categories of Purpose. (Check meaning in real data)
# Age: Best class split at 24, 30, 35. This is turning out to be a weak IV variable. 

# Creating Binary Variables according to the established buckets.
l12_trainsubset <- 
  l12_trainsubset %>%
  mutate(Checking12 = ifelse(Checking==1|Checking==2, 1, 0),
         Checking3 = ifelse(Checking==3, 1, 0),
         History01 = ifelse(History==0|History==1, 1 ,0),
         History2 = ifelse(History==2, 1, 0),
         Property12 = ifelse(Property==1|Property==2, 1, 0),
         Property3 = ifelse(Property==3, 1, 0),
         Duration06 = ifelse(Duration<=6, 1, 0),
         Duration711 = ifelse(Duration>6&Duration<=11, 1, 0))
head(l12_trainsubset)

l12_trainsubset <- select(l12_trainsubset, -c(Checking, History, Property, Duration))

m12_trainsubset <- 
  m12_trainsubset %>%
  mutate(Checking1 = ifelse(Checking==1, 1, 0),
         Checking2 = ifelse(Checking==2, 1, 0),
         Checking3 = ifelse(Checking==3, 1, 0),
         Savings12 = ifelse(Savings==1|Savings==2, 1 ,0),
         Savings3 = ifelse(Savings==3, 1, 0),
         Purpose0 = ifelse(Purpose==0, 1, 0),
         Purpose12 = ifelse(Purpose==1|Purpose==2, 1, 0),
         Age023 = ifelse(Age<=23, 1, 0),
         Age2429 = ifelse(Age>23&Age<=29, 1, 0),
         Age3034 = ifelse(Age>29&Age<=34, 1, 0))
head(m12_trainsubset)

m12_trainsubset <- select(m12_trainsubset, -c(Checking,Savings, Purpose, Age))

l12_infosubset <- create_infotables(l12_trainsubset, NULL, y="Good")
m12_infosubset <- create_infotables(m12_trainsubset, NULL, y="Good")
l12_infosubset$Summary
m12_infosubset

# The IV has shifted based on the binary variables.

# Linear Regression L12
l12_linr <- lm(Good~., data=l12_trainsubset)
vif(l12_linr, merge_coef = TRUE)
# None of the VIF is above 5. This means there is no co-linearity between variables.
l12_linrstep <- step(l12_linr, direction = 'both', trace = FALSE)
l12_linr <- eval(l12_linrstep$call)
summary(l12_linr)

# Linear Regression M12
m12_linr <- lm(Good~., data=m12_trainsubset)
vif(m12_linr, merge_coef = TRUE)
# None of the VIF is above 5. This means there is no co-linearity between variables.
m12_linrstep <- step(m12_linr, direction = 'both', trace = FALSE)
m12_linr <- eval(m12_linrstep$call)
summary(m12_linr)

# Logistic Regression L12
l12_logr <- glm(Good ~ ., family= binomial(), data = l12_trainsubset)
vif(l12_logr, merge_coef = TRUE)
# None of the VIF is above 5. This means there is no co-linearity between variables.
l12_logrstep <- step(l12_logr, direction = 'both', trace = FALSE)
l12_logr <- eval(l12_logrstep$call)
summary(l12_logr)

# Logistic Regression M12
m12_logr <- glm(Good ~ ., family= binomial(), data = m12_trainsubset)
vif(m12_logr, merge_coef = TRUE)
# None of the VIF is above 5. This means there is no co-linearity between variables.
m12_logrstep <- step(m12_logr, direction = 'both', trace = FALSE)
m12_logr <- eval(m12_logrstep$call)
summary(m12_logr)

# Processing the test sets to prepare for evaluation.
l12_testsubset <- l12_test[,c('Checking', 'History', 'Property', 'Duration', 'Good')]
m12_testsubset <- m12_test[,c('Checking', 'Savings', 'Purpose', 'Age', 'Good')]
l12_testsubset <- 
  l12_testsubset %>%
  mutate(Checking12 = ifelse(Checking==1|Checking==2, 1, 0),
         Checking3 = ifelse(Checking==3, 1, 0),
         History01 = ifelse(History==0|History==1, 1 ,0),
         History2 = ifelse(History==2, 1, 0),
         Property12 = ifelse(Property==1|Property==2, 1, 0),
         Property3 = ifelse(Property==3, 1, 0),
         Duration06 = ifelse(Duration<=6, 1, 0),
         Duration711 = ifelse(Duration>6&Duration<=11, 1, 0))
head(l12_testsubset)

l12_testsubset <- select(l12_testsubset, -c(Checking, History, Property, Duration))

m12_testsubset <- 
  m12_testsubset %>%
  mutate(Checking1 = ifelse(Checking==1, 1, 0),
         Checking2 = ifelse(Checking==2, 1, 0),
         Checking3 = ifelse(Checking==3, 1, 0),
         Savings12 = ifelse(Savings==1|Savings==2, 1 ,0),
         Savings3 = ifelse(Savings==3, 1, 0),
         Purpose0 = ifelse(Purpose==0, 1, 0),
         Purpose12 = ifelse(Purpose==1|Purpose==2, 1, 0),
         Age023 = ifelse(Age<=23, 1, 0),
         Age2429 = ifelse(Age>23&Age<=29, 1, 0),
         Age3034 = ifelse(Age>29&Age<=34, 1, 0))
head(m12_testsubset)

m12_testsubset <- select(m12_testsubset, -c(Checking,Savings, Purpose, Age))

l12_traintestsubset <- list(train = l12_trainsubset,test = l12_testsubset)
m12_traintestsubset <- list(train = m12_trainsubset,test = m12_testsubset)
l12_label <- lapply(l12_traintestsubset, function(x) x$Good)
m12_label <- lapply(m12_traintestsubset, function(x) x$Good)

l12_linr_pred <- lapply(l12_traintestsubset, function(x) predict(l12_linr, x, type='response'))
l12_linr_perf <- perf_eva(pred = l12_linr_pred, label=l12_label, binomial_metric = c('ks','gini'),
                 show_plot = 'roc', title = 'Duration <= 12 Linear Regression', confusion_matrix = TRUE)
l12_linr_perf

l12_logr_pred <- lapply(l12_traintestsubset, function(x) predict(l12_logr, x, type='response'))
l12_logr_perf <- perf_eva(pred = l12_logr_pred, label=l12_label, binomial_metric = c('ks','gini'), 
                 show_plot = 'roc', title = 'Duration <= 12 Logistic Regression', confusion_matrix = TRUE)
l12_logr_perf

m12_linr_pred <- lapply(m12_traintestsubset, function(x) predict(m12_linr, x, type='response'))
m12_linr_perf <- perf_eva(pred = m12_linr_pred, label=m12_label, binomial_metric = c('ks','gini'), 
                 show_plot = 'roc', title = 'Duration > 12 Linear Regression', confusion_matrix = TRUE)
m12_linr_perf

m12_logr_pred <- lapply(m12_traintestsubset, function(x) predict(m12_logr, x, type='response'))
m12_logr_perf <- perf_eva(pred = m12_logr_pred, label=m12_label, binomial_metric = c('ks','gini'), 
                 show_plot = 'roc', title = 'Duration > 12 Logistic Regression', confusion_matrix = TRUE)
m12_logr_perf


#The End!