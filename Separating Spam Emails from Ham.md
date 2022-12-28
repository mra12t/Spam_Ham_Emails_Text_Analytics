Separating Spam Emails from Ham
================
2022-12-23

### About the Study and the Data Set

Nearly every email user has at some point received an unsolicited
message, often referred to as spam, which may contain advertisements,
links to malware, or attempts to scam the recipient. A large portion,
roughly 80-90%, of the over 100 billion emails sent daily are spam
emails, which are often sent from botnets of infected computers. The
remaining emails, called “ham” emails, are not spam.

To address the high volume of spam emails being sent, most email
providers offer a spam filter that automatically detects and separates
likely spam messages from the ham. These filters use a variety of
techniques, including checking the sender’s IP address against a list of
known spammers, but most rely heavily on text analytics to analyze the
contents of an email.

In this study, we will build and evaluate a spam filter using a publicly
available dataset described in a 2006 conference paper called “Spam
Filtering with Naive Bayes – Which Naive Bayes?” by V. Metsis, I.
Androutsopoulos, and G. Paliouras. The ham messages in the dataset come
from the inbox of Vincent Kaminski, a former Enron Managing Director for
Research, and are part of the Enron Corpus. The spam messages in the
dataset include those from the SpamAssassin corpus, which consists of
hand-labeled spam contributed by internet users, as well as spam
collected by Project Honey Pot, a project that gathers spam and
identifies spammers by publishing email addresses that humans would not
contact but that bots may target with spam. The full dataset we will use
is a mixture of approximately 75% ham and 25% spam messages.

The dataset consists of two fields:

- text: The content of the email in the form of text.

- spam: A binary variable that indicates whether or not the email is
  spam, with a value of 1 indicating spam and 0 indicating not spam.

### EDA

``` r
emails = read.csv("emails.csv", stringsAsFactors = FALSE)
str(emails)
```

    ## 'data.frame':    5728 obs. of  2 variables:
    ##  $ text: chr  "Subject: naturally irresistible your corporate identity  lt is really hard to recollect a company : the  market"| __truncated__ "Subject: the stock trading gunslinger  fanny is merrill but muzo not colza attainder and penultimate like esmar"| __truncated__ "Subject: unbelievable new homes made easy  im wanting to show you this  homeowner  you have been pre - approved"| __truncated__ "Subject: 4 color printing special  request additional information now ! click here  click here for a printable "| __truncated__ ...
    ##  $ spam: int  1 1 1 1 1 1 1 1 1 1 ...

``` r
library(knitr)
x = table(emails$spam)
kable(x)
```

| Var1 | Freq |
|:-----|-----:|
| 0    | 4360 |
| 1    | 1368 |

``` r
barplot(x)
```

![](Separating%20Spam%20Emails%20from%20Ham_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
emails$text[1]
```

    ## [1] "Subject: naturally irresistible your corporate identity  lt is really hard to recollect a company : the  market is full of suqgestions and the information isoverwhelminq ; but a good  catchy logo , stylish statlonery and outstanding website  will make the task much easier .  we do not promise that havinq ordered a iogo your  company will automaticaily become a world ieader : it isguite ciear that  without good products , effective business organization and practicable aim it  will be hotat nowadays market ; but we do promise that your marketing efforts  will become much more effective . here is the list of clear  benefits : creativeness : hand - made , original logos , specially done  to reflect your distinctive company image . convenience : logo and stationery  are provided in all formats ; easy - to - use content management system letsyou  change your website content and even its structure . promptness : you  will see logo drafts within three business days . affordability : your  marketing break - through shouldn ' t make gaps in your budget . 100 % satisfaction  guaranteed : we provide unlimited amount of changes with no extra fees for you to  be surethat you will love the result of this collaboration . have a look at our  portfolio _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ not interested . . . _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _"

``` r
emails$text[2]
```

    ## [1] "Subject: the stock trading gunslinger  fanny is merrill but muzo not colza attainder and penultimate like esmark perspicuous ramble is segovia not group try slung kansas tanzania yes chameleon or continuant clothesman no  libretto is chesapeake but tight not waterway herald and hawthorn like chisel morristown superior is deoxyribonucleic not clockwork try hall incredible mcdougall yes hepburn or einsteinian earmark no  sapling is boar but duane not plain palfrey and inflexible like huzzah pepperoni bedtime is nameable not attire try edt chronography optima yes pirogue or diffusion albeit no "

``` r
emails$text[3]
```

    ## [1] "Subject: unbelievable new homes made easy  im wanting to show you this  homeowner  you have been pre - approved for a $ 454 , 169 home loan at a 3 . 72 fixed rate .  this offer is being extended to you unconditionally and your credit is in no way a factor .  to take advantage of this limited time opportunity  all we ask is that you visit our website and complete  the 1 minute post approval form  look foward to hearing from you ,  dorcas pittman"

it appears that emails tend to start with the word “Subject”.  
Let’s explore the length of the emails.

``` r
summary(nchar(emails$text))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    13.0   508.8   979.0  1556.8  1894.2 43952.0

``` r
which.max(nchar(emails$text))
```

    ## [1] 2651

``` r
which.min(nchar(emails$text))
```

    ## [1] 1992

``` r
emails$text[which.min(nchar(emails$text))]
```

    ## [1] "Subject: fyi "

``` r
emails$spam[which.min(nchar(emails$text))]
```

    ## [1] 0

Now that we have and idea about the data set, let’s prepare the corpus.

### Preparing the Corpus

``` r
library(tm)
```

    ## Loading required package: NLP

``` r
library(SnowballC)
# Building a New Corpus
corpus = VCorpus(VectorSource(emails$text))
# Converting the Text to Lower Case
corpus = tm_map(corpus, content_transformer(tolower))
# Removing Punctuations from the Corpus
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
# Removing Stop Words
corpus = tm_map(corpus, removeWords, stopwords("english"))
# Stemming the words
corpus = tm_map(corpus, stemDocument)
# Building a Document Term Matrix
dtm = DocumentTermMatrix(corpus)
dtm
```

    ## <<DocumentTermMatrix (documents: 5728, terms: 28687)>>
    ## Non-/sparse entries: 481719/163837417
    ## Sparsity           : 100%
    ## Maximal term length: 24
    ## Weighting          : term frequency (tf)

Let’s reduce the number of terms which is 28687 to a more reasonable
number by reducing dtm to have only terms that appears in at least 5% of
the documents.

``` r
spdtm = removeSparseTerms(dtm, 0.95)
spdtm 
```

    ## <<DocumentTermMatrix (documents: 5728, terms: 330)>>
    ## Non-/sparse entries: 213551/1676689
    ## Sparsity           : 89%
    ## Maximal term length: 10
    ## Weighting          : term frequency (tf)

330 terms seems reasonable, Let’s build the data frame now!

``` r
emailsSparse = as.data.frame(as.matrix(spdtm))
```

Let’s check the most frequent term in the data frame.

``` r
which.max(colSums(emailsSparse))
```

    ## enron 
    ##    92

``` r
emailsSparse$spam = emails$spam
```

Let’s check the most frequent words in the Ham emails.

``` r
tail(sort(colSums(subset(emailsSparse, spam == 0))))
```

    ##     hou    will    vinc subject     ect   enron 
    ##    5569    6802    8531    8625   11417   13388

These appear to be the most frequent terms in the Ham Emails. Now let’s
see the spam ones!

``` r
tail(sort(colSums(subset(emailsSparse, spam == 1))))
```

    ##    mail     com compani    spam    will subject 
    ##     917     999    1065    1368    1450    1577

Since the two lists are quite different, it appears that the frequency
of these words can help us predict whether an email is spam or not.
However, the most frequent terms for the Ham appears to be more
personalized and data-set specific which means that later on, the model
need to be carefully tested before being applied to other persons!

### Splitting the Data Set into Training and Testing Data sets

``` r
library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)
```

### Building a Logistic Regression Model

``` r
GLMmodel = glm(spam ~ ., data = train, family = "binomial")
```

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
summary(GLMmodel)
```

    ## 
    ## Call:
    ## glm(formula = spam ~ ., family = "binomial", data = train)
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -1.011   0.000   0.000   0.000   1.354  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)
    ## (Intercept) -3.082e+01  1.055e+04  -0.003    0.998
    ## `000`        1.474e+01  1.058e+04   0.001    0.999
    ## `2000`      -3.631e+01  1.556e+04  -0.002    0.998
    ## `2001`      -3.215e+01  1.318e+04  -0.002    0.998
    ## `713`       -2.427e+01  2.914e+04  -0.001    0.999
    ## `853`       -1.212e+00  5.942e+04   0.000    1.000
    ## abl         -2.049e+00  2.088e+04   0.000    1.000
    ## access      -1.480e+01  1.335e+04  -0.001    0.999
    ## account      2.488e+01  8.165e+03   0.003    0.998
    ## addit        1.463e+00  2.703e+04   0.000    1.000
    ## address     -4.613e+00  1.113e+04   0.000    1.000
    ## allow        1.899e+01  6.436e+03   0.003    0.998
    ## alreadi     -2.407e+01  3.319e+04  -0.001    0.999
    ## also         2.990e+01  1.378e+04   0.002    0.998
    ## analysi     -2.405e+01  3.860e+04  -0.001    1.000
    ## anoth       -8.744e+00  2.032e+04   0.000    1.000
    ## applic      -2.649e+00  1.674e+04   0.000    1.000
    ## appreci     -2.145e+01  2.762e+04  -0.001    0.999
    ## approv      -1.302e+00  1.589e+04   0.000    1.000
    ## april       -2.620e+01  2.208e+04  -0.001    0.999
    ## area         2.041e+01  2.266e+04   0.001    0.999
    ## arrang       1.069e+01  2.135e+04   0.001    1.000
    ## ask         -7.746e+00  1.976e+04   0.000    1.000
    ## assist      -1.128e+01  2.490e+04   0.000    1.000
    ## associ       9.049e+00  1.909e+04   0.000    1.000
    ## attach      -1.037e+01  1.534e+04  -0.001    0.999
    ## attend      -3.451e+01  3.257e+04  -0.001    0.999
    ## avail        8.651e+00  1.709e+04   0.001    1.000
    ## back        -1.323e+01  2.272e+04  -0.001    1.000
    ## base        -1.354e+01  2.122e+04  -0.001    0.999
    ## begin        2.228e+01  2.973e+04   0.001    0.999
    ## believ       3.233e+01  2.136e+04   0.002    0.999
    ## best        -8.201e+00  1.333e+03  -0.006    0.995
    ## better       4.263e+01  2.360e+04   0.002    0.999
    ## book         4.301e+00  2.024e+04   0.000    1.000
    ## bring        1.607e+01  6.767e+04   0.000    1.000
    ## busi        -4.803e+00  1.000e+04   0.000    1.000
    ## buy          4.170e+01  3.892e+04   0.001    0.999
    ## call        -1.145e+00  1.111e+04   0.000    1.000
    ## can          3.762e+00  7.674e+03   0.000    1.000
    ## case        -3.372e+01  2.880e+04  -0.001    0.999
    ## chang       -2.717e+01  2.215e+04  -0.001    0.999
    ## check        1.425e+00  1.963e+04   0.000    1.000
    ## click        1.376e+01  7.077e+03   0.002    0.998
    ## com          1.936e+00  4.039e+03   0.000    1.000
    ## come        -1.166e+00  1.511e+04   0.000    1.000
    ## comment     -3.251e+00  3.387e+04   0.000    1.000
    ## communic     1.580e+01  8.958e+03   0.002    0.999
    ## compani      4.781e+00  9.186e+03   0.001    1.000
    ## complet     -1.363e+01  2.024e+04  -0.001    0.999
    ## confer      -7.503e-01  8.557e+03   0.000    1.000
    ## confirm     -1.300e+01  1.514e+04  -0.001    0.999
    ## contact      1.530e+00  1.262e+04   0.000    1.000
    ## continu      1.487e+01  1.535e+04   0.001    0.999
    ## contract    -1.295e+01  1.498e+04  -0.001    0.999
    ## copi        -4.274e+01  3.070e+04  -0.001    0.999
    ## corp         1.606e+01  2.708e+04   0.001    1.000
    ## corpor      -8.286e-01  2.818e+04   0.000    1.000
    ## cost        -1.938e+00  1.833e+04   0.000    1.000
    ## cours        1.665e+01  1.834e+04   0.001    0.999
    ## creat        1.338e+01  3.946e+04   0.000    1.000
    ## credit       2.617e+01  1.314e+04   0.002    0.998
    ## crenshaw     9.994e+01  6.769e+04   0.001    0.999
    ## current      3.629e+00  1.707e+04   0.000    1.000
    ## custom       1.829e+01  1.008e+04   0.002    0.999
    ## data        -2.609e+01  2.271e+04  -0.001    0.999
    ## date        -2.786e+00  1.699e+04   0.000    1.000
    ## day         -6.100e+00  5.866e+03  -0.001    0.999
    ## deal        -1.129e+01  1.448e+04  -0.001    0.999
    ## dear        -2.313e+00  2.306e+04   0.000    1.000
    ## depart      -4.068e+01  2.509e+04  -0.002    0.999
    ## deriv       -4.971e+01  3.587e+04  -0.001    0.999
    ## design      -7.923e+00  2.939e+04   0.000    1.000
    ## detail       1.197e+01  2.301e+04   0.001    1.000
    ## develop      5.976e+00  9.455e+03   0.001    0.999
    ## differ      -2.293e+00  1.075e+04   0.000    1.000
    ## direct      -2.051e+01  3.194e+04  -0.001    0.999
    ## director    -1.770e+01  1.793e+04  -0.001    0.999
    ## discuss     -1.051e+01  1.915e+04  -0.001    1.000
    ## doc         -2.597e+01  2.603e+04  -0.001    0.999
    ## don          2.129e+01  1.456e+04   0.001    0.999
    ## done         6.828e+00  1.882e+04   0.000    1.000
    ## due         -4.163e+00  3.532e+04   0.000    1.000
    ## ect          8.685e-01  5.342e+03   0.000    1.000
    ## edu         -2.122e-01  6.917e+02   0.000    1.000
    ## effect       1.948e+01  2.100e+04   0.001    0.999
    ## effort       1.606e+01  5.670e+04   0.000    1.000
    ## either      -2.744e+01  4.000e+04  -0.001    0.999
    ## email        3.833e+00  1.186e+04   0.000    1.000
    ## end         -1.311e+01  2.938e+04   0.000    1.000
    ## energi      -1.620e+01  1.646e+04  -0.001    0.999
    ## engin        2.664e+01  2.394e+04   0.001    0.999
    ## enron       -8.789e+00  5.719e+03  -0.002    0.999
    ## etc          9.470e-01  1.569e+04   0.000    1.000
    ## even        -1.654e+01  2.289e+04  -0.001    0.999
    ## event        1.694e+01  1.851e+04   0.001    0.999
    ## expect      -1.179e+01  1.914e+04  -0.001    1.000
    ## experi       2.460e+00  2.240e+04   0.000    1.000
    ## fax          3.537e+00  3.386e+04   0.000    1.000
    ## feel         2.596e+00  2.348e+04   0.000    1.000
    ## file        -2.943e+01  2.165e+04  -0.001    0.999
    ## final        8.075e+00  5.008e+04   0.000    1.000
    ## financ      -9.122e+00  7.524e+03  -0.001    0.999
    ## financi     -9.747e+00  1.727e+04  -0.001    1.000
    ## find        -2.623e+00  9.727e+03   0.000    1.000
    ## first       -4.666e-01  2.043e+04   0.000    1.000
    ## follow       1.766e+01  3.080e+03   0.006    0.995
    ## form         8.483e+00  1.674e+04   0.001    1.000
    ## forward     -3.484e+00  1.864e+04   0.000    1.000
    ## free         6.113e+00  8.121e+03   0.001    0.999
    ## friday      -1.146e+01  1.996e+04  -0.001    1.000
    ## full         2.125e+01  2.190e+04   0.001    0.999
    ## futur        4.146e+01  1.439e+04   0.003    0.998
    ## gas         -3.901e+00  4.160e+03  -0.001    0.999
    ## get          5.154e+00  9.737e+03   0.001    1.000
    ## gibner       2.901e+01  2.460e+04   0.001    0.999
    ## give        -2.518e+01  2.130e+04  -0.001    0.999
    ## given       -2.186e+01  5.426e+04   0.000    1.000
    ## good         5.399e+00  1.619e+04   0.000    1.000
    ## great        1.222e+01  1.090e+04   0.001    0.999
    ## group        5.264e-01  1.037e+04   0.000    1.000
    ## happi        1.939e-02  1.202e+04   0.000    1.000
    ## hear         2.887e+01  2.281e+04   0.001    0.999
    ## hello        2.166e+01  1.361e+04   0.002    0.999
    ## help         1.731e+01  2.791e+03   0.006    0.995
    ## high        -1.982e+00  2.554e+04   0.000    1.000
    ## home         5.973e+00  8.965e+03   0.001    0.999
    ## hope        -1.435e+01  2.179e+04  -0.001    0.999
    ## hou          6.852e+00  6.437e+03   0.001    0.999
    ## hour         2.478e+00  1.333e+04   0.000    1.000
    ## houston     -1.855e+01  7.305e+03  -0.003    0.998
    ## howev       -3.449e+01  3.562e+04  -0.001    0.999
    ## http         2.528e+01  2.107e+04   0.001    0.999
    ## idea        -1.845e+01  3.892e+04   0.000    1.000
    ## immedi       6.285e+01  3.346e+04   0.002    0.999
    ## import      -1.859e+00  2.236e+04   0.000    1.000
    ## includ      -3.454e+00  1.799e+04   0.000    1.000
    ## increas      6.476e+00  2.329e+04   0.000    1.000
    ## industri    -3.160e+01  2.373e+04  -0.001    0.999
    ## info        -1.255e+00  4.857e+03   0.000    1.000
    ## inform       2.078e+01  8.549e+03   0.002    0.998
    ## interest     2.698e+01  1.159e+04   0.002    0.998
    ## intern      -7.991e+00  3.351e+04   0.000    1.000
    ## internet     8.749e+00  1.100e+04   0.001    0.999
    ## interview   -1.640e+01  1.873e+04  -0.001    0.999
    ## invest       3.201e+01  2.393e+04   0.001    0.999
    ## invit        4.304e+00  2.215e+04   0.000    1.000
    ## involv       3.815e+01  3.315e+04   0.001    0.999
    ## issu        -3.708e+01  3.396e+04  -0.001    0.999
    ## john        -5.326e-01  2.856e+04   0.000    1.000
    ## join        -3.824e+01  2.334e+04  -0.002    0.999
    ## juli        -1.358e+01  3.009e+04   0.000    1.000
    ## just        -1.021e+01  1.114e+04  -0.001    0.999
    ## kaminski    -1.812e+01  6.029e+03  -0.003    0.998
    ## keep         1.867e+01  2.782e+04   0.001    0.999
    ## kevin       -3.779e+01  4.738e+04  -0.001    0.999
    ## know         1.277e+01  1.526e+04   0.001    0.999
    ## last         1.046e+00  1.372e+04   0.000    1.000
    ## let         -2.763e+01  1.462e+04  -0.002    0.998
    ## life         5.812e+01  3.864e+04   0.002    0.999
    ## like         5.649e+00  7.660e+03   0.001    0.999
    ## line         8.743e+00  1.236e+04   0.001    0.999
    ## link        -6.929e+00  1.345e+04  -0.001    1.000
    ## list        -8.692e+00  2.149e+03  -0.004    0.997
    ## locat        2.073e+01  1.597e+04   0.001    0.999
    ## london       6.745e+00  1.642e+04   0.000    1.000
    ## long        -1.489e+01  1.934e+04  -0.001    0.999
    ## look        -7.031e+00  1.563e+04   0.000    1.000
    ## lot         -1.964e+01  1.321e+04  -0.001    0.999
    ## made         2.820e+00  2.743e+04   0.000    1.000
    ## mail         7.584e+00  1.021e+04   0.001    0.999
    ## make         2.901e+01  1.528e+04   0.002    0.998
    ## manag        6.014e+00  1.445e+04   0.000    1.000
    ## mani         1.885e+01  1.442e+04   0.001    0.999
    ## mark        -3.350e+01  3.208e+04  -0.001    0.999
    ## market       7.895e+00  8.012e+03   0.001    0.999
    ## may         -9.434e+00  1.397e+04  -0.001    0.999
    ## mean         6.078e-01  2.952e+04   0.000    1.000
    ## meet        -1.063e+00  1.263e+04   0.000    1.000
    ## member       1.381e+01  2.343e+04   0.001    1.000
    ## mention     -2.279e+01  2.714e+04  -0.001    0.999
    ## messag       1.716e+01  2.562e+03   0.007    0.995
    ## might        1.244e+01  1.753e+04   0.001    0.999
    ## model       -2.292e+01  1.049e+04  -0.002    0.998
    ## monday      -1.034e+00  3.233e+04   0.000    1.000
    ## money        3.264e+01  1.321e+04   0.002    0.998
    ## month       -3.727e+00  1.112e+04   0.000    1.000
    ## morn        -2.645e+01  3.403e+04  -0.001    0.999
    ## move        -3.834e+01  3.011e+04  -0.001    0.999
    ## much         3.775e-01  1.392e+04   0.000    1.000
    ## name         1.672e+01  1.322e+04   0.001    0.999
    ## need         8.437e-01  1.221e+04   0.000    1.000
    ## net          1.256e+01  2.197e+04   0.001    1.000
    ## new          1.003e+00  1.009e+04   0.000    1.000
    ## `next`       1.492e+01  1.724e+04   0.001    0.999
    ## note         1.446e+01  2.294e+04   0.001    0.999
    ## now          3.790e+01  1.219e+04   0.003    0.998
    ## number      -9.622e+00  1.591e+04  -0.001    1.000
    ## offer        1.174e+01  1.084e+04   0.001    0.999
    ## offic       -1.344e+01  2.311e+04  -0.001    1.000
    ## one          1.241e+01  6.652e+03   0.002    0.999
    ## onlin        3.589e+01  1.665e+04   0.002    0.998
    ## open         2.114e+01  2.961e+04   0.001    0.999
    ## oper        -1.696e+01  2.757e+04  -0.001    1.000
    ## opportun    -4.131e+00  1.918e+04   0.000    1.000
    ## option      -1.085e+00  9.325e+03   0.000    1.000
    ## order        6.533e+00  1.242e+04   0.001    1.000
    ## origin       3.226e+01  3.818e+04   0.001    0.999
    ## part         4.594e+00  3.483e+04   0.000    1.000
    ## particip    -1.154e+01  1.738e+04  -0.001    0.999
    ## peopl       -1.864e+01  1.439e+04  -0.001    0.999
    ## per          1.367e+01  1.273e+04   0.001    0.999
    ## person       1.870e+01  9.575e+03   0.002    0.998
    ## phone       -6.957e+00  1.172e+04  -0.001    1.000
    ## place        9.005e+00  3.661e+04   0.000    1.000
    ## plan        -1.830e+01  6.320e+03  -0.003    0.998
    ## pleas       -7.961e+00  9.484e+03  -0.001    0.999
    ## point        5.498e+00  3.403e+04   0.000    1.000
    ## posit       -1.543e+01  2.316e+04  -0.001    0.999
    ## possibl     -1.366e+01  2.492e+04  -0.001    1.000
    ## power       -5.643e+00  1.173e+04   0.000    1.000
    ## present     -6.163e+00  1.278e+04   0.000    1.000
    ## price        3.428e+00  7.850e+03   0.000    1.000
    ## problem      1.262e+01  9.763e+03   0.001    0.999
    ## process     -2.957e-01  1.191e+04   0.000    1.000
    ## product      1.016e+01  1.345e+04   0.001    0.999
    ## program      1.444e+00  1.183e+04   0.000    1.000
    ## project      2.173e+00  1.497e+04   0.000    1.000
    ## provid       2.422e-01  1.859e+04   0.000    1.000
    ## public      -5.250e+01  2.341e+04  -0.002    0.998
    ## put         -1.052e+01  2.681e+04   0.000    1.000
    ## question    -3.467e+01  1.859e+04  -0.002    0.999
    ## rate        -3.112e+00  1.319e+04   0.000    1.000
    ## read        -1.527e+01  2.145e+04  -0.001    0.999
    ## real         2.046e+01  2.358e+04   0.001    0.999
    ## realli      -2.667e+01  4.640e+04  -0.001    1.000
    ## receiv       5.765e-01  1.585e+04   0.000    1.000
    ## recent      -2.067e+00  1.780e+04   0.000    1.000
    ## regard      -3.668e+00  1.511e+04   0.000    1.000
    ## relat       -5.114e+01  1.793e+04  -0.003    0.998
    ## remov        2.325e+01  2.484e+04   0.001    0.999
    ## repli        1.538e+01  2.916e+04   0.001    1.000
    ## report      -1.482e+01  1.477e+04  -0.001    0.999
    ## request     -1.232e+01  1.167e+04  -0.001    0.999
    ## requir       5.004e-01  2.937e+04   0.000    1.000
    ## research    -2.826e+01  1.553e+04  -0.002    0.999
    ## resourc     -2.735e+01  3.522e+04  -0.001    0.999
    ## respond      2.974e+01  3.888e+04   0.001    0.999
    ## respons     -1.960e+01  3.667e+04  -0.001    1.000
    ## result      -5.002e-01  3.140e+04   0.000    1.000
    ##  [ reached getOption("max.print") -- omitted 81 rows ]
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 4409.49  on 4009  degrees of freedom
    ## Residual deviance:   13.46  on 3679  degrees of freedom
    ## AIC: 675.46
    ## 
    ## Number of Fisher Scoring iterations: 25

Non of the variables is significant for our logistic regression model!

``` r
# the Model probabilities on the training set
GLMpredtrain = predict(GLMmodel, type = "response")
```

``` r
table(GLMpredtrain < 0.00001)
```

    ## 
    ## FALSE  TRUE 
    ##   964  3046

``` r
table(GLMpredtrain > 0.99999)
```

    ## 
    ## FALSE  TRUE 
    ##  3056   954

``` r
table(GLMpredtrain > 0.00001 & GLMpredtrain < 0.99999)
```

    ## 
    ## FALSE  TRUE 
    ##  4000    10

``` r
# Accuracy on the training set
x = table(train$spam, GLMpredtrain >= 0.5)
kable(x)
```

|     | FALSE | TRUE |
|:----|------:|-----:|
| 0   |  3052 |    0 |
| 1   |     4 |  954 |

``` r
(x[1]+x[4])/sum(x)
```

    ## [1] 0.9990025

99.9%!.

``` r
library(ROCR)
#AUC 
predtrain = prediction(GLMpredtrain, train$spam)
as.numeric(performance(predtrain, "auc")@y.values)
```

    ## [1] 0.9999959

0.99 AUC as well!

It appears that our model really found a good relationship between the
independent variables in the training set! Let’s see how well it
performs on the testing set.

``` r
GLMpredtest = predict(GLMmodel, newdata = test, type = "response")
x = table(test$spam, GLMpredtest >= 0.5)
kable(x)
```

|     | FALSE | TRUE |
|:----|------:|-----:|
| 0   |  1257 |   51 |
| 1   |    34 |  376 |

``` r
# Accuracy 
(x[1]+x[4])/sum(x)
```

    ## [1] 0.9505239

95% accuracy on the testing set! let’s check the AUC.

``` r
#auc
predtest = prediction(GLMpredtest, test$spam)
as.numeric(performance(predtest, "auc")@y.values)
```

    ## [1] 0.9627517

0.96 AUC!  
Let’s further explore the other option to make a comparision between
them!

### Building a CART Model

``` r
library(rpart)
library(rpart.plot)
CARTmodel = rpart(spam ~., data = train, method = "class")
prp(CARTmodel)
```

![](Separating%20Spam%20Emails%20from%20Ham_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
# Making Prediction
CARTpredtrain = predict(CARTmodel)
CARTpredtest = predict(CARTmodel, newdata = test)
# 
x = table(train$spam, CARTpredtrain[,2] >= 0.5)
y = table(test$spam, CARTpredtest[,2] >= 0.5)
kable(x)
```

|     | FALSE | TRUE |
|:----|------:|-----:|
| 0   |  2885 |  167 |
| 1   |    64 |  894 |

``` r
kable(y)
```

|     | FALSE | TRUE |
|:----|------:|-----:|
| 0   |  1228 |   80 |
| 1   |    24 |  386 |

``` r
# Accuracy and AUC on the train set of CART model
(x[1]+x[4])/sum(x)
```

    ## [1] 0.942394

``` r
predCARTtrain = prediction(CARTpredtrain[,2], train$spam)
as.numeric(performance(predCARTtrain, "auc")@y.values)
```

    ## [1] 0.9696044

On the training set, the accuracy of the CART model is 94% and the area
under the curve is 0.97!

``` r
# Accuracy and AUC on the test set of CART model
(y[1]+y[4])/sum(y)
```

    ## [1] 0.9394645

``` r
predCARTtest = prediction(CARTpredtest[,2], test$spam)
as.numeric(performance(predCARTtest, "auc")@y.values)
```

    ## [1] 0.963176

On the testing set, the accuracy of the CART model is almost 94% and the
area under the curve is 0.96!

### Building a Random Forest Model

``` r
# altering the column name to make sure the random forest function runs properly
train2 = train
test2 = test
colnames(train2) = paste0("F", colnames(train2))
colnames(test2) = paste0("F", colnames(test2))
# converting column to factor
train2$Fspam = as.factor(train2$Fspam)
```

``` r
library(randomForest)
```

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

``` r
set.seed(123)
RFmodel = randomForest(Fspam ~., data = train2)
```

``` r
# Making Prediction
RFpredtrain = predict(RFmodel, type = "prob")
RFpredtest = predict(RFmodel, newdata = test2, type = "prob")
# 
x = table(train2$Fspam, RFpredtrain[,2] >= 0.5)
y = table(test2$Fspam, RFpredtest[,2] >= 0.5)
kable(x)
```

|     | FALSE | TRUE |
|:----|------:|-----:|
| 0   |  3014 |   38 |
| 1   |    41 |  917 |

``` r
kable(y)
```

|     | FALSE | TRUE |
|:----|------:|-----:|
| 0   |  1290 |   18 |
| 1   |    23 |  387 |

``` r
# Accuracy and AUC on the train set of Random Forest model
(x[1]+x[4])/sum(x)
```

    ## [1] 0.9802993

``` r
predRFtrain = prediction(RFpredtrain[,2], train2$Fspam)
as.numeric(performance(predRFtrain, "auc")@y.values)
```

    ## [1] 0.9978155

On the training set, the accuracy of the RF model is 98% and the area
under the curve is 0.99!

``` r
# Accuracy and AUC on the test set of Random Forest model
(y[1]+y[4])/sum(y)
```

    ## [1] 0.976135

``` r
predRFtest = prediction(RFpredtest[,2], test2$Fspam)
as.numeric(performance(predRFtest, "auc")@y.values)
```

    ## [1] 0.9975899

On the testing set, the accuracy of the RF model is almost 98% and the
area under the curve is 0.99!

### Summary

In summary, when comparing the accuracy and the AUC of the three model
on the training set, we found that the logistic regression model had the
best performance. However, given that the near-perfect solution of the
model didn’t stand the test, and produced not-perfect results on the
testing set, we can conclude that this is a sign of ***Over Fitting***
for the model. Also, we conclude that the random forest model is the
best performer on the testing set in terms of both accuracy and AUC.

### A Problem-Specific Approach

So far, we have used a threshold of 0.5 and accuracy as our measure of
model quality when predicting whether an email is spam or not. This
means that if the model’s prediction is greater than 0.5, it is
classified as spam, and if it is less than 0.5, it is classified as not
spam. Using a threshold of 0.5 and accuracy as our measure is a good
choice when we don’t have a preference for one type of error over the
other (false positives versus false negatives). However, if we assign a
higher cost to one type of error, we may want to consider using a
different threshold or a different measure of model quality.

Imagine that we are using the spam filter we have developed for an email
provider. The provider moves all emails flagged as spam to a separate
“Junk Email” folder, while all other emails are displayed in the main
inbox. Many users of this email provider may never check the spam
folder, meaning they will never see emails that are classified as spam
by the filter and delivered to that folder. In this case, it might be
more important to minimize false negatives (emails classified as not
spam that should have been classified as spam) rather than false
positives (emails classified as spam that should have been classified as
not spam), as false negatives could lead to users missing important
emails. In this case, we might want to consider using a higher threshold
(e.g. 0.7) to minimize the number of false negatives, at the cost of
potentially increasing the number of false positives.

***The cost associated with a false negative error is a spam email
showing up in the inbox of the user causing an incontinence to the user.
However, the cost of a false positive error is an important email ending
up in the Junk mail where the user will miss and not see***

***Hence, a False Positive is more costly, and less desirable***

***Ideally, if the email provider collects data about how frequently the
user check their junk mail, this data can be utilized to best use the
model. For instance, if the user doesn’t check their junk mail what so
ever, we can let some emails go to the main mail while flagging them
“potential scam”, therefore, decreasing the cost of false positives!***

### Building a Possibly Better Model

``` r
wordCounts = rowSums(as.matrix(dtm))
```

``` r
hist(wordCounts)
```

![](Separating%20Spam%20Emails%20from%20Ham_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

The distribution is right-skewed, meaning there are more wordCounts of
smaller values.

``` r
hist(log(wordCounts))
```

![](Separating%20Spam%20Emails%20from%20Ham_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

In the histogram of the logarithmic scale of the date, it appears that
we don’t have skewness.

``` r
emailsSparse$LogWordsCount = log(wordCounts)
```

``` r
boxplot(emailsSparse$LogWordsCount~emailsSparse$spam)
```

![](Separating%20Spam%20Emails%20from%20Ham_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

We can see that the 1st quartile, median, and 3rd quartiles are all
slightly lower for spam messages than for ham messages.

``` r
colnames(emailsSparse) = paste0("F", colnames(emailsSparse))
colnames(emailsSparse) = paste0("F", colnames(emailsSparse))
```

``` r
train3 = subset(emailsSparse, spl == TRUE)
test3 = subset(emailsSparse, spl == FALSE)
```

``` r
# New CART Model
CARTmodel2 = rpart(FFspam ~., data = train3)
prp(CARTmodel2)
```

![](Separating%20Spam%20Emails%20from%20Ham_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->
It appears that LogWords did become a split in the new CART model.

``` r
train3$FFspam = as.factor(train3$FFspam)
set.seed(123)
RFmodel2 = randomForest(FFspam ~ ., data = train3)
```

``` r
# CART Prediction
CARTpredtest2 = predict(CARTmodel2, newdata = test3)
# RF Prediction
RFpredtest2 = predict(RFmodel2, newdata = test3, type = "prob")

x = table(test3$FFspam, CARTpredtest2 >= 0.5)
y = table(test3$FFspam, RFpredtest2[,2] >= 0.5)

kable(x)
```

|     | FALSE | TRUE |
|:----|------:|-----:|
| 0   |  1242 |   66 |
| 1   |    40 |  370 |

``` r
kable(y)
```

|     | FALSE | TRUE |
|:----|------:|-----:|
| 0   |  1298 |   10 |
| 1   |    29 |  381 |

``` r
# CART Accuracy and AUC
(x[1]+x[4])/sum(x)
```

    ## [1] 0.9383003

``` r
predCARTtest2 = prediction(CARTpredtest2, test3$FFspam)
as.numeric(performance(predCARTtest2, "auc")@y.values)
```

    ## [1] 0.9679654

``` r
# RF Accuracy and AUC
(y[1]+y[4])/sum(y)
```

    ## [1] 0.9772992

``` r
predRFtest2 = prediction(RFpredtest2[,2], test3$FFspam)
as.numeric(performance(predRFtest2, "auc")@y.values)
```

    ## [1] 0.9979162

***We can see that for the new CART model, the accuracy still almost 94%
and the AUC is 0.97. And the accuracy of the Random forest model has
remained at almost 98%!***

***In conclusion, the addition of the new variable which is the word
count, or frankly the log of it, to the model, did not improve our
models after all***
