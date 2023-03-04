#### Maximal sporophyte length ####
Length <- read.csv("~/Desktop/Projects/QUB/Pressure/Length.csv")
Length <- Length[Length$Length != 0,] 
# zeros should not be included here because an absent sporophyte 
# does not mean that it had a length of zero

Length$Pressure <- as.numeric(Length$Pressure)
require(lme4)
m1 <- lmer(Length ~ Pressure + (1|Flask), data = Length)
# I include Flask as a random intercept in this model because flasks
# were the true statistical units in the experiment

plot(resid(m1) ~ Pressure, data = Length)
abline(0,0) # heterogenous because residual variance decreases with pressure

hist(resid(m1)) # balanced but not normal

m2 <- glmer(Length ~ Pressure + (1|Flask), 
            family = Gamma(link = "log"), 
            data = Length)
plot(resid(m2) ~ Pressure, data = Length)
abline(0,0) # homogenous

hist(resid(m2)) # normal
# m2 is chosen as the optimal model for the length data

require(car)
Anova(m2) # Type II Wald Chi-square test
# Response: Length
#           Chisq Df Pr(>Chisq)   
# Pressure 7.6004  1   0.005836 **

summary(m2)
# Random effects:
# Groups   Name        Variance Std.Dev.
# Flask    (Intercept) 0.09513  0.3084  
# Residual             0.07557  0.2749  
# Number of obs: 101, groups:  Flask, 21
# 
# Fixed effects:
#              Estimate Std. Error t value Pr(>|z|)   
# (Intercept)   0.1231     0.2683   0.459  0.64645   
# Pressure     -0.2341     0.0849  -2.757  0.00584 **

# y = exp(-0.2341*x + 0.1231) because exp() is the inverse of log()
  
coef(m2) # coefficients for each flask
#      (Intercept)   Pressure
# 1.1 -0.460494074 -0.2340629
# 1.2 -0.092610804 -0.2340629
# 1.3  0.320282537 -0.2340629
# 1.4  0.010542359 -0.2340629
# 1.5  0.269356280 -0.2340629
# 1.6  0.308800635 -0.2340629
# 2.2 -0.014885416 -0.2340629
# 2.3 -0.277964730 -0.2340629
# 2.4  0.903100965 -0.2340629
# 2.5  0.714998349 -0.2340629
# 2.6 -0.404113905 -0.2340629
# 3.2 -0.014772837 -0.2340629
# 3.5  1.102436024 -0.2340629
# 3.6  0.201427397 -0.2340629
# 4.2 -0.218594719 -0.2340629
# 4.5  0.272597710 -0.2340629
# 5.1 -0.244427505 -0.2340629
# 5.2  0.306427681 -0.2340629
# 5.3 -0.034626452 -0.2340629
# 5.4  0.364762763 -0.2340629
# 5.5  0.001689931 -0.2340629

# descriptive statistics at the flask (replicate) level
lstat <- data.frame(Pressure = c(rep(1,6),rep(2,5),rep(3,3),rep(4,2),rep(5,5)),
                    Flask = aggregate(Length ~ Flask, mean, data = Length)[,1],
                    Mean = aggregate(Length ~ Flask, mean, data = Length)[,2],
                    SD = aggregate(Length ~ Flask, sd, data = Length)[,2],
                    n = aggregate(Length ~ Flask, length, data = Length)[,2])
lstat$SE <- with(lstat, SD/sqrt(n))

# descriptive statistics at the treatment level
ltstat <- data.frame(Pressure = aggregate(Mean ~ Pressure, mean, data = lstat)[,1],
                     Mean = aggregate(Mean ~ Pressure, mean, data = lstat)[,2],
                     SD = aggregate(Mean ~ Pressure, sd, data = lstat)[,2],
                     n = aggregate(Mean ~ Pressure, length, data = lstat)[,2])
ltstat$SE <- with(ltstat, SD/sqrt(n))
ltstat


lnew <- expand.grid(Pressure = seq(1, 5, by = 0.1),
                    Length = 0)

linv <- family(m2)$linkinv # inverse of the link function
lnew$fit <- predict(m2, type = "link", re.form = NA, newdata = lnew) # fit on link scale

lmm <- model.matrix(terms(m2), lnew)
fvar <- diag(lmm %*% tcrossprod(vcov(m2), lmm)) # fixed uncertainty
tvar <- fvar + VarCorr(m2)$Flask[1] # total (fixed + random) uncertainty

lnew$upper <- linv(lnew$fit + qnorm(0.975)*sqrt(tvar))
lnew$lower <- linv(lnew$fit - qnorm(0.975)*sqrt(tvar))
lnew$fit <- linv(lnew$fit)


require(ggplot2)

mytheme <- theme(panel.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 plot.margin = unit(c(.2, .3, .2, .2),"cm"),
                 axis.line = element_line(),
                 axis.title = element_text(size = 15),
                 axis.text = element_text(size = 12, colour = "black"),
                 axis.ticks.length = unit(.25, "cm"),
                 axis.ticks = element_line(colour = "black"),
                 legend.key = element_blank(),
                 legend.key.size = unit(.3, "cm"),
                 legend.key.height = unit(.45, "cm"),
                 legend.spacing.x = unit(.1, "cm"),
                 legend.spacing.y = unit(.05, "cm"),
                 legend.background = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.text.align = 0,
                 legend.title.align = 0,
                 legend.title = element_text(size = 12, face = "bold"),
                 text = element_text(family = "Helvetica Neue"))

lp <- ggplot() +
  geom_point(data = Length, aes(Pressure, Length, group = factor(Flask)), shape = 16, size = 3, 
             position = position_dodge(width = 0.6), colour = "#b6b12d", alpha = 0.5) +
  geom_ribbon(data = lnew, aes(Pressure, ymin = lower, ymax = upper), alpha = 0.3, fill = "#b6b12d") +
  geom_line(data = lnew, aes(Pressure, fit), colour = "#b6b12d") +
  geom_pointrange(data = lstat, aes(Pressure, Mean, ymin = Mean - SE*qnorm(0.975), 
                                    ymax = Mean + SE*qnorm(0.975), 
                                    group = factor(Flask)), colour = "#b6b12d",
                  position = position_dodge(width = 0.6)) +
  annotate("text", x = c(4.5, 4.5), y = c(2.9, 2.5), size = 4.2, hjust = 0, parse = T,
           label = c("italic('N ')*'= 101 (21)'", "italic('y ')*'= '*e^{-0.23*italic('x ')*'+ '*0.12}"), 
           family = "Helvetica Neue") +
  labs(x = "Pressure (bar)", y = "Sporophyte length (mm)") +
  coord_cartesian(xlim = c(0, 6), ylim = c(0, 3), expand = FALSE) +
  mytheme

lp

#### Sporophyte cover ####
Cover <- read.csv("~/Desktop/Projects/QUB/Pressure/Cover.csv")

m3 <- glmer(Cover ~ Pressure + (1|Flask),
            family = binomial(link = "logit"),
            data = Cover)
# I include Flask as a random intercept in this model because flasks
# were the true statistical units in the experiment

plot(resid(m3) ~ Pressure, data = Cover)
abline(0,0) # homogenous
# m3 is chosen as the optimal model for the cover data

Anova(m3) # Type II Wald Chi-square test
# Response: Cover
#          Chisq Df Pr(>Chisq)    
# Pressure 13.36  1   0.000257 ***

summary(m3)
# Random effects:
# Groups Name        Variance Std.Dev.
# Flask  (Intercept) 0.2766   0.5259  
# Number of obs: 900, groups:  Flask, 30
# 
# Fixed effects:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.11750    0.28413  -0.414 0.679216    
# Pressure    -0.32665    0.08937  -3.655 0.000257 ***

# y = 1/(1 + exp(0.32665*x + 0.11750)) because the logistic function is
# the inverse of the logit (log-odds) function

coef(m3) # coefficients for each flask
#     (Intercept)   Pressure
# 1.1 -0.18501557 -0.3266475
# 1.2 -0.09175939 -0.3266475
# 1.3  0.36211156 -0.3266475
# 1.4  0.09148945 -0.3266475
# 1.5 -0.18501557 -0.3266475
# 1.6 -0.27973223 -0.3266475
# 2.1 -0.16620424 -0.3266475
# 2.2  0.12460428 -0.3266475
# 2.3 -0.37197424 -0.3266475
# 2.4  0.21810342 -0.3266475
# 2.5 -0.37197424 -0.3266475
# 2.6 -0.16620424 -0.3266475
# 3.1 -0.06454562 -0.3266475
# 3.2 -0.28478526 -0.3266475
# 3.3 -0.92028129 -0.3266475
# 3.4  0.52792604 -0.3266475
# 3.5 -0.28478526 -0.3266475
# 3.6  0.14227420 -0.3266475
# 4.1  0.02114345 -0.3266475
# 4.2 -0.76736576 -0.3266475
# 4.3  0.13389856 -0.3266475
# 4.4 -0.47919210 -0.3266475
# 4.5  0.45066909 -0.3266475
# 4.6 -0.61957372 -0.3266475
# 5.1  0.85925130 -0.3266475
# 5.2  0.32692891 -0.3266475
# 5.3  0.08595925 -0.3266475
# 5.4  0.08595925 -0.3266475
# 5.5 -0.46831320 -0.3266475
# 5.6 -0.79189105 -0.3266475


# descriptive statistics at the flask (replicate) level
cstat <- aggregate(Cover ~ Flask, length, data = Cover)
colnames(cstat)[2] <- "n"
cstat$Ones <- aggregate(Cover ~ Flask, function(x) sum(x == 1), data = Cover)$Cover

require(binom) # calculate Wilson 95% CI (for SE enter (pnorm(1)-0.5)*2 for conf.level)
binom <- with(cstat, 
              binom.confint(Ones, n, conf.level = 0.95,     
                            method = "wilson", type = "central"))[,4:6] 

cstat <- cbind(cstat, binom)
cstat$Pressure <- c(rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6))
  
  
# descriptive statistics at the treatment level
ctstat <- aggregate(Cover ~ Pressure, length, data = Cover)
colnames(ctstat)[2] <- "n"
ctstat$Ones <- aggregate(Cover ~ Pressure, function(x) sum(x == 1), data = Cover)$Cover

binom <- with(ctstat, 
              binom.confint(Ones, n, conf.level = (pnorm(1)-0.5)*2,     
                            method = "wilson", type = "central"))[,4:6] 

ctstat <- cbind(ctstat, binom)
ctstat
# these Wilson SE may be underestimates due to artificial sample size inflation
# because they are based on technical replicates; this needs to be checked by 
# re-calculating SE using only statistical replicates

# Wilson SE can only be calculated from binomial data with binom.confint(), 
# not from proportions,but statistical replicates are proportions in his case, 
# so Wald SE are used for cross-validation

ctstat2 <- data.frame(Pressure = aggregate(mean ~ Pressure, mean, data = cstat)[,1],
                      Mean = aggregate(mean ~ Pressure, mean, data = cstat)[,2],
                      n = aggregate(mean ~ Pressure, length, data = cstat)[,2])
ctstat2$SE <- with(ctstat2, sqrt(Mean*(1-Mean)/n)) # Wald SE for proportion
ctstat2$compSEu <- with(ctstat, upper - mean) 
ctstat2$compSEl <- with(ctstat, mean - lower) # estimate comparative SEs from ctstat
ctstat2
# Wilson SE is clearly underestimated compared to the Wald SE, so there is 
# clear evidence of underestimation of SE due to artificial sample size 
# inflation as was expected because Wilson SE is based on technical replicates
# while Wald SE is based on statistical replicates


binom.confint(2.5, 6, conf.level = (pnorm(1)-0.5)*2,     
              method = "wilson", type = "central")

# descriptive statistics at the treatment level
ctstat <-  data.frame(Pressure = aggregate(mean ~ Pressure, mean, data = cstat)[,1],
                      Ones = aggregate(mean ~ Pressure, mean, data = cstat)[,2] *
                             aggregate(mean ~ Pressure, length, data = cstat)[,2],
                      n = aggregate(mean ~ Pressure, length, data = cstat)[,2])
binom <- with(ctstat, 
              binom.confint(Ones, n, conf.level = (pnorm(1)-0.5)*2,     
                            method = "wilson", type = "central"))[,4:6] 

ctstat <- cbind(ctstat, binom)
ctstat




  
cnew <- expand.grid(Pressure = seq(1, 5, by = 0.1),
                    Cover = 0)

cinv <- family(m3)$linkinv # inverse of the link function
cnew$fit <- predict(m3, type = "link", re.form = NA, newdata = cnew) # fit on link scale

cmm <- model.matrix(terms(m3), cnew)
fvar <- diag(cmm %*% tcrossprod(vcov(m3), cmm)) # fixed uncertainty
tvar <- fvar + VarCorr(m3)$Flask[1] # total (fixed + random) uncertainty

cnew$upper <- cinv(cnew$fit + qnorm(0.975)*sqrt(tvar))
cnew$lower <- cinv(cnew$fit - qnorm(0.975)*sqrt(tvar))
cnew$fit <- cinv(cnew$fit)

cp <- ggplot() +
  geom_ribbon(data = cnew, aes(Pressure, ymin = lower*100, ymax = upper*100), 
  fill = "#b6b12d", alpha = 0.3) +
  geom_line(data = cnew, aes(Pressure, fit*100), colour = "#b6b12d") +
  geom_pointrange(data = cstat, aes(Pressure, mean*100, ymin = lower*100, ymax = upper*100, 
                                    group = factor(Flask)), colour = "#b6b12d",
                  position = position_dodge(width = 0.6)) +
  annotate("text", x = c(4.5, 4.5), y = c(97, 83), size = 4.2, hjust = 0, parse = T,
           label = c("italic('N ')*'= 900 (30)'", "italic('y ')*'= '*frac(100, 1+e^{0.33*italic('x ')*'+ '*0.12})"), 
           family = "Helvetica Neue") +
  labs(x = "Pressure (bar)", y = "Sporophyte cover (%)") +
  coord_cartesian(xlim = c(0, 6), ylim = c(0, 100), expand = FALSE) +
  mytheme

cp

require(cowplot)
cp <- cp + theme(axis.title.x = element_blank(),
                 axis.text.x = element_blank())
clp <- plot_grid(cp, lp, nrow = 2, labels = "auto", align = "v", rel_heights = c(0.895,1),
                 label_size = 15, label_fontfamily = "Helvetica Neue")
clp # dimensions: 6 x 6 in
