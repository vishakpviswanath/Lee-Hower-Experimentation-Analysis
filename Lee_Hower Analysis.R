rm(list=ls())   
# read csv file
data <- read.csv('Lee_Hower_Sales.csv')
# data summary
summary(data)

# treatment and control group 
treatment <- subset(data, data$treat == 1)
control <- subset(data, data$treat == 0)

#Exposed treatment and control group

treat_exposed <- subset(treatment,treatment$saw_ads == 1)
control_exposed <- subset(control, control$saw_ads == 1)

### step 1: randomization check:
# difference of the mean
diff_female = mean(treatment$female) - mean(control$female)

# calculate standard error of the difference
se1_female = sd(treatment$female)/sqrt(nrow(treatment)) 
se2_female = sd(control$female)/sqrt(nrow(control))
se_diff_female = sqrt(se1_female^2 + se2_female^2)

# calculate 95% CI for the difference
ci1_female = diff_female - 1.96*se_diff_female
ci2_female = diff_female + 1.96*se_diff_female
# see if CI includes zero
ci1_female
ci2_female  #CI includes 0 thus randomization check passed

#past_sales

diff_presale = mean(treatment$past_sales) - mean(control$past_sales)   # difference of the mean for presales
se1_presale = sd(treatment$past_sales)/sqrt(nrow(treatment))
se2_presale = sd(control$past_sales)/sqrt(nrow(control))
se_diff_presale = sqrt(se1_presale^2 + se2_presale^2)              # se of the difference for presales
ci1_presale = diff_presale - 1.96*se_diff_presale
ci2_presale = diff_presale + 1.96*se_diff_presale                  # 95% CI for the difference for presales
ci1_presale
ci2_presale                                             #CI includes 0 thus randomization check passed

### step 2: calculate ITT estimate
# difference of the mean

diff_sale = mean(treatment$sales) - mean(control$sales)

# se of the difference
se1_sale = sd(treatment$sales)/sqrt(nrow(treatment))
se2_sale = sd(control$sales)/sqrt(nrow(control))
se_sale = sqrt(se1_sale^2 + se2_sale^2)

# calculate 95% CI for the difference
ci1_itt = diff_sale - 1.96*se_sale
ci2_itt = diff_sale + 1.96*se_sale
round(ci1_itt,4)
round(ci2_itt,4)                                    #CI includes 0 this could be due to noise in the data.

### step 3: control ad validation
# difference of the mean for female percentage
diff_female_exposed = round(mean(treat_exposed$female) - mean(control_exposed$female),4)

# se of the difference
se1_female_exposed = sd(treat_exposed$female)/sqrt(nrow(treat_exposed))
se2_female_exposed = sd(control_exposed$female)/sqrt(nrow(control_exposed))
se_female_exposed = sqrt(se1_female_exposed^2 + se2_female_exposed^2)

# calculate 95% CI for the difference
ci1_female_exposed = diff_female_exposed - 1.96*se_female_exposed
ci2_female_exposed = diff_female_exposed + 1.96*se_female_exposed
ci1_female_exposed
ci2_female_exposed

# do the same for past_sales
diff_presale_exposed = mean(treat_exposed$past_sales) - mean(control_exposed$past_sales)       # difference of the mean for exposed presales
se1_presale_exposed = sd(treat_exposed$past_sales)/sqrt(nrow(treat_exposed))
se2_presale_exposed = sd(control_exposed$past_sales)/sqrt(nrow(control_exposed))
se_diff_presale_exposed = sqrt(se1_presale_exposed^2 + se2_presale_exposed^2)              # se of the difference for exposed presales
round(se_diff_presale_exposed,4)# calculate 95% CI for the difference

ci1_presale_exposed = diff_presale_exposed - 1.96*se_diff_presale_exposed
ci2_presale_exposed = diff_presale_exposed + 1.96*se_diff_presale_exposed                  # 95% CI for the difference for exposed presales
ci1_presale_exposed
ci2_presale_exposed

### step 4: calculate TOT estimate
# difference of the mean
diff_sale_exposed = round(mean(treat_exposed$sales) - mean(control_exposed$sales),4)
diff_sale_exposed
# se of the difference
se1_sale_exposed = sd(treat_exposed$sales)/sqrt(nrow(treat_exposed))
se2_sale_exposed = sd(control_exposed$sales)/sqrt(nrow(control_exposed))
se_sale_exposed = sqrt(se1_sale_exposed^2 + se2_sale_exposed^2)
round(se_sale_exposed,4)
# calculate 95% CI for the difference
ci1_sale_exposed = diff_sale_exposed - 1.96*se_sale_exposed
ci2_sale_exposed = diff_sale_exposed + 1.96*se_sale_exposed
ci1_sale_exposed
ci2_sale_exposed

### calculate the total effects of the campaign, gross margin is 50%
tt_itt = diff_sale*nrow(treatment)*0.5
tt_tot = diff_sale_exposed*nrow(treat_exposed)*0.5

round(tt_itt,4)
round(tt_tot,4)
