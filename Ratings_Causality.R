# Setting the Working Directory
setwd("~/UniLu/the")

takehome_ratings <- read.csv("takehome_ratings.csv",   ";",header= T)



View(takehome_ratings)
dim(takehome_ratings)
str(takehome_ratings)
summary(takehome_ratings)

# 1)	In my dataset (takehome_ratings.csv), I first calculate the individual causal effect (ICE) 
# by subtracting the potential income under high ratings from the potential 
# income under low ratings for each unit.


library(dplyr) 
bla <- takehome_ratings

takehome <- takehome_ratings %>%  mutate(bla$y1 - bla$y0) %>% rename( ice = "bla$y1 - bla$y0")

# 2)	Before I calculate the causal effect of the individual groups, I first calculate the difference
# between the outcomes of the groups that received a treatment and the groups that did not receive a treatment.

ATE_wrong <- sum(takehome$ice) / nrow(takehome)

# 3)	Now I calculate the average treatment effect for the 
# Treatment Group and Control Group.

# Number in the treatment group
countTG <- nrow(filter(takehome, d == 1))
countTG
# Number in the control group
countCG <-  nrow(filter(takehome, d == 0))
countCG

ATT_Filter <- filter(takehome, d == 1)
ATT_wrong <- sum(ATT_Filter$ice) / countTG
ATT_wrong

ATC_Filter <- filter(takehome, d == 0)
ATC_wrong <- sum(ATC_Filter$ice) / countCG
ATC_wrong

# 4)	Check if there are any Selection Bias: 

potential_outcome_under_control_1 <- sum(ATT_Filter$y0) / countTG
potential_outcome_under_control_1
potential_outcome_under_control_0 <- sum(ATC_Filter$y0) / countCG
potential_outcome_under_control_0
Selection_bias <-  potential_outcome_under_control_1 - potential_outcome_under_control_0
Selection_bias

# 5)	Additional: Calculate the Naive estimate: 

naive_estimate <- ATT_wrong + Selection_bias
naive_estimate

# 6)	Check if we forgot a variable
# Now lets heck whether the variable Quality was not considered (omitted variable test)

# Like exercise one. Both Means are different, so we can say that it is not colider, but cofounder!
mean(takehome$y[takehome$d==0])
mean(takehome$y[takehome$d==1])


# Because this is a coufounder, we have to take the onmitted variable into account as well.
# As we can see here, the coefficient d has slightly decreased. For this we have the variable Quality,
# which can also help explain this model now. As we can see here, the R-Squared has improved a bit.
# However, the F statistics have decreased a bit. The significance remains about the same for both.

(lm(y ~ d, takehome))
summary(lm(y ~ d + quality, takehome))






# 7)	Use various randomization measures.
#   Bernoulli trials:
#   Completely randomized experiment:
#   Completely randomized experiment within each block of the variable quality:
#   Stratified randomized experiment:


takehome_randomized <- takehome 
library(randomizr)
set.seed(12345)
takehome_randomized$bernoulli <- simple_ra(9934, prob=0.5)
takehome_randomized$complrandom <- complete_ra(N=9934, m=9934/2)
takehome_randomized$block <- block_ra(blocks = takehome_randomized$quality, prob = 0.5)
takehome_randomized$stratified <- strata_rs(strata = takehome_randomized$quality, prob = 0.5)

# 8)	Execute steps 3 to 4 again for all radon nominations.

rm(takehome_randomized)

# Above I have created four columns, which performs the assignment using Radnomization. But I prefer it if I
# only have one column and overwrite it! 
# For this reason I will now display both

takehome_randomized <- takehome 

set.seed(12345)
takehome_randomized$assignment <-simple_ra(9934, prob=0.5)
             # Now I calculate the average treatment effect for the 
              # Treatment Group and Control Group.
              
              # Number in the treatment group
              countTG <- nrow(filter(takehome_randomized, assignment == 1))
              countTG
              # Number in the control group
              countCG <-  nrow(filter(takehome_randomized, assignment == 0))
              countCG
              
              ATT_Filter <- filter(takehome_randomized, assignment == 1)
              ATT_wrong <- sum(ATT_Filter$ice) / countTG
              ATT_wrong
              
              ATC_Filter <- filter(takehome_randomized, assignment == 0)
              ATC_wrong <- sum(ATC_Filter$ice) / countCG
              ATC_wrong
              
              # )	Check if there are any Selection Bias: 
              
              potential_outcome_under_control_1 <- sum(ATT_Filter$y0) / countTG
              potential_outcome_under_control_1
              potential_outcome_under_control_0 <- sum(ATC_Filter$y0) / countCG
              potential_outcome_under_control_0
              Selection_bias <-  potential_outcome_under_control_1 - potential_outcome_under_control_0
              Selection_bias





set.seed(12345)
takehome_randomized$assignment <-complete_ra(N=9934, m=9934/2)
              
              # Now I calculate the average treatment effect for the 
              # Treatment Group and Control Group.
              
              # Number in the treatment group
              countTG <- nrow(filter(takehome_randomized, assignment == 1))
              countTG
              # Number in the control group
              countCG <-  nrow(filter(takehome_randomized, assignment == 0))
              countCG
              
              ATT_Filter <- filter(takehome_randomized, assignment == 1)
              ATT_wrong <- sum(ATT_Filter$ice) / countTG
              ATT_wrong
              
              ATC_Filter <- filter(takehome_randomized, assignment == 0)
              ATC_wrong <- sum(ATC_Filter$ice) / countCG
              ATC_wrong
              
              # )	Check if there are any Selection Bias: 
              
              potential_outcome_under_control_1 <- sum(ATT_Filter$y0) / countTG
              potential_outcome_under_control_1
              potential_outcome_under_control_0 <- sum(ATC_Filter$y0) / countCG
              potential_outcome_under_control_0
              Selection_bias <-  potential_outcome_under_control_1 - potential_outcome_under_control_0
              Selection_bias
  
set.seed(12345)
takehome_randomized$assignment <-block_ra(blocks = takehome_randomized$quality, prob = 0.5)
              # Now I calculate the average treatment effect for the 
              # Treatment Group and Control Group.
              
              # Number in the treatment group
              countTG <- nrow(filter(takehome_randomized, assignment == 1))
              countTG
              # Number in the control group
              countCG <-  nrow(filter(takehome_randomized, assignment == 0))
              countCG
              
              ATT_Filter <- filter(takehome_randomized, assignment == 1)
              ATT_wrong <- sum(ATT_Filter$ice) / countTG
              ATT_wrong
              
              ATC_Filter <- filter(takehome_randomized, assignment == 0)
              ATC_wrong <- sum(ATC_Filter$ice) / countCG
              ATC_wrong
              
              # 4)	Check if there are any Selection Bias: 
              
              potential_outcome_under_control_1 <- sum(ATT_Filter$y0) / countTG
              potential_outcome_under_control_1
              potential_outcome_under_control_0 <- sum(ATC_Filter$y0) / countCG
              potential_outcome_under_control_0
              Selection_bias <-  potential_outcome_under_control_1 - potential_outcome_under_control_0
              Selection_bias
set.seed(12345)
takehome_randomized$assignment <-strata_rs(strata = takehome_randomized$quality, prob = 0.5)

              
              # Now I calculate the average treatment effect for the 
              # Treatment Group and Control Group.
              
              # Number in the treatment group
              countTG <- nrow(filter(takehome_randomized, assignment == 1))
              countTG
              # Number in the control group
              countCG <-  nrow(filter(takehome_randomized, assignment == 0))
              countCG
              
              ATT_Filter <- filter(takehome_randomized, assignment == 1)
              ATT_wrong <- sum(ATT_Filter$ice) / countTG
              ATT_wrong
              
              ATC_Filter <- filter(takehome_randomized, assignment == 0)
              ATC_wrong <- sum(ATC_Filter$ice) / countCG
              ATC_wrong
              
              # 4)	Check if there are any Selection Bias: 
              
              potential_outcome_under_control_1 <- sum(ATT_Filter$y0) / countTG
              potential_outcome_under_control_1
              potential_outcome_under_control_0 <- sum(ATC_Filter$y0) / countCG
              potential_outcome_under_control_0
              Selection_bias <-  potential_outcome_under_control_1 - potential_outcome_under_control_0
              Selection_bias
       
 
              
              
              #########################################################################################################################################
              #                                    Exercise 2 - Matching (14 points)                                                                                                   #   
              #                                                                                                                                       #
              #                                                                                                                                       #
              ########################################################################################################################################                
              
              library(dplyr)
              library(tidyverse)
              library(MatchIt)
              library("cobalt")
              setwd("~/UniLu/the")
              load("schools.RData")
              schools
              school <- schools
              head(schools)
              view(schools)
              school <- school %>% mutate( if_else(homework > 3, 1, 0)) %>% rename( treat = "if_else(homework > 3, 1, 0)")
              
              # IDs interessieren mich nicht!
              school[1:2] <- list(NULL)
              
              school %>% group_by(treat) %>% summarise_all(funs(mean))
              
              # 2)	I start with the general comparison by comparing all columns by grouping by treatment.
              sex <- school %>% group_by(treat, sex) %>% summarise_all(funs(mean))
              
              parented <-  school %>% group_by(treat, parented) %>% summarise_all(funs(mean))
              sex <- school %>% group_by(treat, sex) %>% summarise_all(funs(mean))
              race <-  school %>% group_by(treat, race) %>% summarise_all(funs(mean))
              sctype <- school %>% group_by(treat, sctype) %>% summarise_all(funs(mean))
              urban <- school %>% group_by(treat, urban) %>% summarise_all(funs(mean))
              region <- school %>% group_by(treat, region) %>% summarise_all(funs(mean))
              school %>% group_by(treat, homework) %>% summarise_all(funs(mean))
              
              #Let the matching begin
              # For this reason, the propensity score is now calculated, whereby it is calculated for three
              # different distance metrics
              # logistically, probalistically and mahalanobis. (Steps 1 to 4 from page 29 of lecture 6 are performed)
              
              #Logit as distance metric
              ##################################################################################################################
              
              psm.1_l <- matchit(treat~parented + sex + race + sctype + urban +region +ses + ratio,
                                 data=school, method = "nearest", ratio=1, distance = "logit")
              psm.1_l    
              love.plot(psm.1_l) # Absolute mean difference should be closer to 0
              summary(psm.1_l)
              plot(psm.1_l, type = "hist")
              plot(psm.1_l, type = "jitter") 
              
              
              
              matcheddata1_l <- match.data(psm.1_l)
              matcheddata1_l
              summary(lm(math~treat, matcheddata1_l))
              
              
              
              psm.2_l <- matchit(treat~parented + sex + race + sctype + urban +region +ses + ratio,
                                 data=school, method = "nearest", ratio=2, distance = "logit")
              psm.2_l # Unmatched 154, way too much
              love.plot(psm.2_l) # Sehr schlecht, die angepasteten Punkte sind sehr weit entfertn von 0 
              summary(psm.2_l)
              
              
              matcheddata2_l <- match.data(psm.2_l)
              matcheddata2_l
              summary(lm(math~treat, matcheddata2_l))
              
              
              
              psm.3_l <- matchit(treat~parented + sex + race + sctype + urban +region +ses + ratio,
                                 data=school, method = "nearest", ratio=3, distance = "logit")
              psm.3_l
              love.plot(psm.3_l)    # Unmatched 154, way too much
              summary(psm.3_l)
              
              
              matcheddata3_l <- match.data(psm.3_l)
              matcheddata3_l
              summary(lm(math~treat, matcheddata3_l))
              
              
              psm.4_l <- matchit(treat~parented + sex + race + sctype + urban +region +ses + ratio,
                                 data=school, method = "nearest", ratio=4, distance = "logit")
              psm.4_l
              love.plot(psm.4_l)
              summary(psm.4_l)
              
              
              matcheddata4_l <- match.data(psm.4_l)
              matcheddata4_l
              summary(lm(math~treat, matcheddata4_l))
              
              #Probit as distance metric
              ##################################################################################################################
              
              psm.1_p <- matchit(treat~parented + sex + race + sctype + urban +region +ses + ratio,
                                 data=school, method = "nearest", ratio=1, distance = "probit")
              psm.1_p
              love.plot(psm.1_p)
              summary(psm.1_p)
              plot(psm.1_p, type = "hist")
              plot(psm.1_p, type = "jitter") 
              
              matcheddata1_p <- match.data(psm.1_p)
              matcheddata1_p
              summary(lm(math~treat, matcheddata1_p))
              
              
              
              psm.2_p <- matchit(treat~parented + sex + race + sctype + urban +region +ses + ratio,
                                 data=school, method = "nearest", ratio=2, distance = "probit")
              psm.2_p
              love.plot(psm.2_p)
              summary(psm.2_p)
              
              
              matcheddata2_p <- match.data(psm.2_p)
              matcheddata2_p
              summary(lm(math~treat, matcheddata2_p))
              
              
              psm.3_p <- matchit(treat~parented + sex + race + sctype + urban +region +ses + ratio,
                                 data=school, method = "nearest", ratio=3, distance = "probit")
              psm.3_p
              love.plot(psm.3_p)
              summary(psm.3_p)
              
              
              matcheddata3_p <- match.data(psm.3_p)
              matcheddata3_p
              summary(lm(math~treat, matcheddata3_p))
              
              
              psm.4_p <- matchit(treat~parented + sex + race + sctype + urban +region +ses + ratio,
                                 data=school, method = "nearest", ratio=4, distance = "probit")
              psm.4_p
              love.plot(psm.4_p)
              summary(psm.4_p)
              
              
              matcheddata4_p <- match.data(ppsm.4_p)
              matcheddata4_p
              summary(lm(math~treat, matcheddata4_p))
              
              
              #Mahalanobis as distance metric
              ##################################################################################################################
              psm.1_m <- matchit(treat~parented + sex + race + sctype + urban +region +ses + ratio,
                                 data=school, method = "nearest", ratio=1, distance = "mahalanobis")
              
              psm.1_m 
              love.plot(psm.1_m )
              summary(psm.1_m )
              
              matcheddata1_m <- match.data(psm.1_m )
              matcheddata1_m
              summary(lm(math~treat, matcheddata1_m))
              
              
              
              psm.2_m <- matchit(treat~parented + sex + race + sctype + urban +region +ses + ratio,
                                 data=school, method = "nearest", ratio=2, distance = "mahalanobis")
              psm.2_m 
              love.plot(psm.2_m )
              summary(psm.2_m )
              
              matcheddata2_m <- match.data(psm.2_m )
              matcheddata2_m
              summary(lm(math~treat, matcheddata2_m))
              
              
              
              psm.3_m <- matchit(treat~parented + sex + race + sctype + urban +region +ses + ratio,
                                 data=school, method = "nearest", ratio=3, distance = "mahalanobis")
              psm.3_m
              love.plot(psm.3_m)
              summary(psm.3_m)
              
              
              matcheddata3_m <- match.data(psm.3_m)
              matcheddata3_m
              summary(lm(math~treat, matcheddata3_m))
              
              
              psm.4_m <- matchit(treat~parented + sex + race + sctype + urban +region +ses + ratio,
                                 data=school, method = "nearest", ratio=4, distance = "mahalanobis")
              psm.4_m
              love.plot(psm.4_m)
              summary(psm.4_m)
              
              matcheddata4_m <- match.data(psm.4_m)
              matcheddata4_m
              summary(lm(math~treat, matcheddata4_m))
              
              
              
              
              
              # 6. Calculate the effect of the treatment on the outcome in the matched dataset (I've tried something here.)
              
              library("Zelig")
              
              
              # 
              
              
              z.out1 <- zelig(math ~ parented + sex + race + sctype + urban + region +ses + ratio , data = match.data(psm.1_l, "control"), model = "ls")
              
              x.out1 <- setx(z.out1, data = match.data(psm.1_l, "treat"), cond = TRUE)
              
              
              
              s.out1 <- sim(z.out1, x = x.out1)
              
              
              
              
             
              
