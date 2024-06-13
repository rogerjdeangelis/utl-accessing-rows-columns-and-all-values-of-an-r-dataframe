%let pgm=utl-accessing-rows-columns-and-all-values-of-an-r-dataframe;

Accessing row columns and  all values of an r dataframe


https://tinyurl.com/2pazrbmu
https://github.com/rogerjdeangelis/utl-accessing-rows-columns-and-all-values-of-an-r-dataframe

Convert your sas code to R and Python
https://tinyurl.com/y687w5hw
https://github.com/rogerjdeangelis/utl-converting-common-wps-coding-to-r-and-python

https://tinyurl.com/3t5m6aj9
https://github.com/rogerjdeangelis/utl-leveraging-your-knowledge-of-perl-regex-to-sas-wps-r-python-and-perl

Alos see
https://r-guru.com/r-cheat-sheets
https://github.com/gto76/python-cheatsheet

I am not presenting access by rowname because row names are not used in other languages.

AI prompts (I use perplexity (seems to hvae fewer false positives?)
where can i find r tip or cheat sheets.
where can i find python tip ot cheat sheets.

/*                   _
(_)_ __  _ __  _   _| |_
| | `_ \| `_ \| | | | __|
| | | | | |_) | |_| | |_
|_|_| |_| .__/ \__,_|\__|
        |_|
*/
libname sd1 "d:/sd1";
options validvarname=upcase;
data sd1.have;
  set sashelp.iris(drop=species
       obs=4
       rename=(
         SEPALLENGTH =  SLENGTH
         SEPALWIDTH  =  SWIDTH
         PETALLENGTH =  PLENGTH
         PETALWIDTH  =  PWIDTH )

       );
run;quit;

/**************************************************************************************************************************/
/*                                                                                                                        */
/*  SD1.HAVE total obs=4 12JUN2024:17:34:53                                                                               */
/*                                                                                                                        */
/*  SLENGTH    SWIDTH    PLENGTH    PWIDTH                                                                                */
/*                                                                                                                        */
/*     50        33         14         2                                                                                  */
/*     46        34         14         3                                                                                  */
/*     46        36         10         2                                                                                  */
/*     51        33         17         5                                                                                  */
/*                                                                                                                        */
/**************************************************************************************************************************/

/*
 _ __  _ __ ___   ___ ___  ___ ___
| `_ \| `__/ _ \ / __/ _ \/ __/ __|
| |_) | | | (_) | (_|  __/\__ \__ \
| .__/|_|  \___/ \___\___||___/___/
|_|
*/
options ls=255;
%utl_rbegin;
parmcards4;
library(dplyr)
library(haven)
library(sqldf)
have<-read_sas("d:/sd1/have.sas7bdat")
# [R,C]    1       2       3        4
#       SLENGTH  SWIDTH  PLENGTH  PWIDTH
#
#  1       50      33       14       2
#  2       46      34       14       3
#  3       46      36       10       2
#  4       51      33       17       5
#           _                         _                   _       _
# _   _ ___(_)_ __   __ _   ___ _   _| |__  ___  ___ _ __(_)_ __ | |_ ___
#| | | / __| | `_ \ / _` | / __| | | | `_ \/ __|/ __| `__| | `_ \| __/ __|
#| |_| \__ \ | | | | (_| | \__ \ |_| | |_) \__ \ (__| |  | | |_) | |_\__ \
# \__,_|___/_|_| |_|\__, | |___/\__,_|_.__/|___/\___|_|  |_| .__/ \__|___/
#                   |___/                                  |_|
#
have[4, 1] # Access element in 4th row, 1st column
have[1:3,1:2] # rows 1 through 3 and columns 1 and 2
have[4,] # row 4
have[4:1,] # reverse rows
have[-1,] # drop first row
have[-1:-2,] # drop first two rows
sum(have[1,4:4]) # sum height and weight in row 1
have[c(2,4,1,3),] # scmnble rows
have[c(1,1,2,2,3,3,4,4,4,4),] # double all rows
cbind(have,c(AVGLEN=(have[,1]+have[,3])/2)) # add avg length (slength+plength)/2
cbind(have,c(AVGWTH=(have[,2]+have[,4])/2)) # add avg width (swidth+pwidth)/2
have[have[,1]>50,] # subset SLENGTH > 50
sqrt(have) # square root of each element of have
dag<-data.frame(x=double());
for ( i in seq(1,nrow(have),1)) {
  dag[i,]<-have[i,i]
  }
dag # diagonal elements of the have dataframe;
cbind(have,AVGLEN=(have[,1]+have[,3])/2) # add  column with length average
rbind(have,colSums(have)) # add row with column sums
cbind(have,ROWSUM=rowSums(have)) # add column withn row sums
#           _
#  ___ ___ | |_   _ _ __ ___  _ __    _ __   __ _ _ __ ___   ___  ___
# / __/ _ \| | | | | `_ ` _ \| `_ \  | `_ \ / _` | `_ ` _ \ / _ \/ __|
#| (_| (_) | | |_| | | | | | | | | | | | | | (_| | | | | | |  __/\__ \
# \___\___/|_|\__,_|_| |_| |_|_| |_| |_| |_|\__,_|_| |_| |_|\___||___/
#
#
have$AVGLEN<-(have$SLENGTH+have$PLENGTH)/2 # add avg length (slength+plength)/2
have[have$SLENGTH>49,] # filter rows have$SLENGTH>49
have<-read_sas("d:/sd1/have.sas7bdat")
rowcol <- sqldf('
   select
      *
     ,(slength+swidth+plength+pwidth) as rowsum
   from
      have
   union
      all
   select
      sum(slength)
      ,sum(swidth)
      ,sum(plength)
      ,sum(pwidth)
      ,null
   from
     have
   ')
rowcol # all row and column sums
;;;;
%utl_rend;

/*           _               _
  ___  _   _| |_ _ __  _   _| |_
 / _ \| | | | __| `_ \| | | | __|
| (_) | |_| | |_| |_) | |_| | |_
 \___/ \__,_|\__| .__/ \__,_|\__|
                |_|
*/

/**************************************************************************************************************************/
/*                                                                                                                        */
/*  > h ave<-read_sas("d:/sd1/have.sas7bdat")                                                                             */
/*  > # [R,C]    1       2       3        4                                                                               */
/*  > #       SLENGTH  SWIDTH  PLENGTH  PWIDTH                                                                            */
/*  > #                                                                                                                   */
/*  > #  1       50      33       14       2                                                                              */
/*  > #  2       46      34       14       3                                                                              */
/*  > #  3       46      36       10       2                                                                              */
/*  > #  4       51      33       17       5                                                                              */
/*  > #           _                         _                   _       _                                                 */
/*  > # _   _ ___(_)_ __   __ _   ___ _   _| |__  ___  ___ _ __(_)_ __ | |_ ___                                           */
/*  > #| | | / __| | `_ \ / _` | / __| | | | `_ \/ __|/ __| `__| | `_ \| __/ __|                                          */
/*  > #| |_| \__ \ | | | | (_| | \__ \ |_| | |_) \__ \ (__| |  | | |_) | |_\__ \                                          */
/*  > # \__,_|___/_|_| |_|\__, | |___/\__,_|_.__/|___/\___|_|  |_| .__/ \__|___/                                          */
/*  > #                   |___/                                  |_|                                                      */
/*                                                                                                                        */
/*  > have[4, 1] # Access element in 4th row, 1st column                                                                  */
/*                                                                                                                        */
/*    SLENGTH                                                                                                             */
/*      <dbl>                                                                                                             */
/*  1      51                                                                                                             */
/*                                                                                                                        */
/*  > have[1:3,1:2] # rows 1 through 3 and columns 1 and 2                                                                */
/*                                                                                                                        */
/*    SLENGTH SWIDTH                                                                                                      */
/*      <dbl>  <dbl>                                                                                                      */
/*  1      50     33                                                                                                      */
/*  2      46     34                                                                                                      */
/*  3      46     36                                                                                                      */
/*                                                                                                                        */
/*  > have[4,] # row 4                                                                                                    */
/*                                                                                                                        */
/*    SLENGTH SWIDTH PLENGTH PWIDTH                                                                                       */
/*      <dbl>  <dbl>   <dbl>  <dbl>                                                                                       */
/*  1      51     33      17      5                                                                                       */
/*                                                                                                                        */
/*  > have[4:1,] # reverse rows                                                                                           */
/*                                                                                                                        */
/*    SLENGTH SWIDTH PLENGTH PWIDTH                                                                                       */
/*      <dbl>  <dbl>   <dbl>  <dbl>                                                                                       */
/*  1      51     33      17      5                                                                                       */
/*  2      46     36      10      2                                                                                       */
/*  3      46     34      14      3                                                                                       */
/*  4      50     33      14      2                                                                                       */
/*                                                                                                                        */
/*  > have[-1,] # drop first row                                                                                          */
/*                                                                                                                        */
/*    SLENGTH SWIDTH PLENGTH PWIDTH                                                                                       */
/*      <dbl>  <dbl>   <dbl>  <dbl>                                                                                       */
/*  1      46     34      14      3                                                                                       */
/*  2      46     36      10      2                                                                                       */
/*  3      51     33      17      5                                                                                       */
/*                                                                                                                        */
/*  > have[-1:-2,] # drop first two rows                                                                                  */
/*                                                                                                                        */
/*    SLENGTH SWIDTH PLENGTH PWIDTH                                                                                       */
/*      <dbl>  <dbl>   <dbl>  <dbl>                                                                                       */
/*  1      46     36      10      2                                                                                       */
/*  2      51     33      17      5                                                                                       */
/*                                                                                                                        */
/*  > have[c(2,4,1,3),] # scmnble rows                                                                                    */
/*                                                                                                                        */
/*    SLENGTH SWIDTH PLENGTH PWIDTH                                                                                       */
/*      <dbl>  <dbl>   <dbl>  <dbl>                                                                                       */
/*  1      46     34      14      3                                                                                       */
/*  2      51     33      17      5                                                                                       */
/*  3      50     33      14      2                                                                                       */
/*  4      46     36      10      2                                                                                       */
/*                                                                                                                        */
/*  > have[c(1,1,2,2,3,3,4,4,4,4),] # double all rows                                                                     */
/*                                                                                                                        */
/*     SLENGTH SWIDTH PLENGTH PWIDTH                                                                                      */
/*       <dbl>  <dbl>   <dbl>  <dbl>                                                                                      */
/*   1      50     33      14      2                                                                                      */
/*   2      50     33      14      2                                                                                      */
/*   3      46     34      14      3                                                                                      */
/*   4      46     34      14      3                                                                                      */
/*   5      46     36      10      2                                                                                      */
/*   6      46     36      10      2                                                                                      */
/*   7      51     33      17      5                                                                                      */
/*   8      51     33      17      5                                                                                      */
/*   9      51     33      17      5                                                                                      */
/*  10      51     33      17      5                                                                                      */
/*                                                                                                                        */
/*  > cbind(have,c(AVGLEN=(have[,1]+have[,3])/2)) # add avg length (slength+plength)/2                                    */
/*                                                                                                                        */
/*    SLENGTH SWIDTH PLENGTH PWIDTH AVGLEN.SLENGTH                                                                        */
/*  1      50     33      14      2             32                                                                        */
/*  2      46     34      14      3             30                                                                        */
/*  3      46     36      10      2             28                                                                        */
/*  4      51     33      17      5             34                                                                        */
/*                                                                                                                        */
/*  > cbind(have,c(AVGWTH=(have[,2]+have[,4])/2)) # add avg width (swidth+pwidth)/2                                       */
/*                                                                                                                        */
/*    SLENGTH SWIDTH PLENGTH PWIDTH AVGWTH.SWIDTH                                                                         */
/*  1      50     33      14      2          17.5                                                                         */
/*  2      46     34      14      3          18.5                                                                         */
/*  3      46     36      10      2          19.0                                                                         */
/*  4      51     33      17      5          19.0                                                                         */
/*                                                                                                                        */
/*  > have[have[,1]>50,] # subset SLENGTH > 50                                                                            */
/*                                                                                                                        */
/*    SLENGTH SWIDTH PLENGTH PWIDTH                                                                                       */
/*      <dbl>  <dbl>   <dbl>  <dbl>                                                                                       */
/*  1      51     33      17      5                                                                                       */
/*                                                                                                                        */
/*  > sqrt(have) # square root of each element of have                                                                    */
/*                                                                                                                        */
/*    SLENGTH SWIDTH PLENGTH PWIDTH                                                                                       */
/*      <dbl>  <dbl>   <dbl>  <dbl>                                                                                       */
/*  1    7.07   5.74    3.74   1.41                                                                                       */
/*  2    6.78   5.83    3.74   1.73                                                                                       */
/*  3    6.78   6       3.16   1.41                                                                                       */
/*  4    7.14   5.74    4.12   2.24                                                                                       */
/*                                                                                                                        */
/*  > dag<-data.frame(x=double());                                                                                        */
/*  > for ( i in seq(1,nrow(have),1)) {                                                                                   */
/*  +   dag[i,]<-have[i,i]                                                                                                */
/*  +   }                                                                                                                 */
/*  > dag # diagonal elements of the have dataframe;                                                                      */
/*     x                                                                                                                  */
/*  1 50                                                                                                                  */
/*  2 34                                                                                                                  */
/*  3 10                                                                                                                  */
/*  4  5                                                                                                                  */
/*                                                                                                                        */
/*  > cbind(have,AVGLEN=(have[,1]+have[,3])/2) # add  column with length average                                          */
/*                                                                                                                        */
/*    SLENGTH SWIDTH PLENGTH PWIDTH  AVGLEN                                                                               */
/*  1      50     33      14      2      32                                                                               */
/*  2      46     34      14      3      30                                                                               */
/*  3      46     36      10      2      28                                                                               */
/*                                                                                                                        */
/*  4      51     33      17      5      34                                                                               */
/*                                                                                                                        */
/*  > rbind(have,colSums(have)) # add row with column sums                                                                */
/*                                                                                                                        */
/*    SLENGTH SWIDTH PLENGTH PWIDTH                                                                                       */
/*      <dbl>  <dbl>   <dbl>  <dbl>                                                                                       */
/*  1      50     33      14      2                                                                                       */
/*  2      46     34      14      3                                                                                       */
/*  3      46     36      10      2                                                                                       */
/*  4      51     33      17      5                                                                                       */
/*                                                                                                                        */
/*  5     193    136      55     12                                                                                       */
/*                                                                                                                        */
/*                                                                                                                        */
/*  > cbind(have,ROWSUM=rowSums(have)) # add column withn row sums                                                        */
/*                                                                                                                        */
/*    SLENGTH SWIDTH PLENGTH PWIDTH ROWSUM                                                                                */
/*  1      50     33      14      2     99                                                                                */
/*  2      46     34      14      3     97                                                                                */
/*  3      46     36      10      2     94                                                                                */
/*  4      51     33      17      5    106                                                                                */
/*  > #           _                                                                                                       */
/*  > #  ___ ___ | |_   _ _ __ ___  _ __    _ __   __ _ _ __ ___   ___  ___                                               */
/*  > # / __/ _ \| | | | | `_ ` _ \| `_ \  | `_ \ / _` | `_ ` _ \ / _ \/ __|                                              */
/*  > #| (_| (_) | | |_| | | | | | | | | | | | | | (_| | | | | | |  __/\__ \                                              */
/*  > # \___\___/|_|\__,_|_| |_| |_|_| |_| |_| |_|\__,_|_| |_| |_|\___||___/                                              */
/*  > #                                                                                                                   */
/*  > #                                                                                                                   */
/*  > have$AVGLEN<-(have$SLENGTH+have$PLENGTH)/2 # add avg length (slength+plength)/2                                     */
/*  > have[have$SLENGTH>49,] # filter rows have$SLENGTH>49                                                                */
/*                                                                                                                        */
/*    SLENGTH SWIDTH PLENGTH PWIDTH AVGLEN                                                                                */
/*      <dbl>  <dbl>   <dbl>  <dbl>  <dbl>                                                                                */
/*  1      50     33      14      2     32                                                                                */
/*  2      51     33      17      5     34                                                                                */
/*                                                                                                                        */
/*  > have<-read_sas("d:/sd1/have.sas7bdat")                                                                              */
/*                                                                                                                        */
/*  > rowcol <- sqldf('                                                                                                   */
/*  +    select                                                                                                           */
/*  +       *                                                                                                             */
/*  +      ,(slength+swidth+plength+pwidth) as rowsum                                                                     */
/*  +    from                                                                                                             */
/*  +       have                                                                                                          */
/*  +    union                                                                                                            */
/*  +       all                                                                                                           */
/*  +    select                                                                                                           */
/*  +       sum(slength)                                                                                                  */
/*  +       ,sum(swidth)                                                                                                  */
/*  +       ,sum(plength)                                                                                                 */
/*  +       ,sum(pwidth)                                                                                                  */
/*  +       ,null                                                                                                         */
/*  +    from                                                                                                             */
/*  +      have                                                                                                           */
/*  +    ')                                                                                                               */
/*  > rowcol # all row and column sums                                                                                    */
/*                                                                                                                        */
/*    SLENGTH SWIDTH PLENGTH PWIDTH rowsum                                                                                */
/*  1      50     33      14      2     99                                                                                */
/*  2      46     34      14      3     97                                                                                */
/*  3      46     36      10      2     94                                                                                */
/*  4      51     33      17      5    106                                                                                */
/*  5     193    136      55     12     NA                                                                                */
/*  >                                                                                                                     */
/*                                                                                                                        */
/**************************************************************************************************************************/

/*              _
  ___ _ __   __| |
 / _ \ `_ \ / _` |
|  __/ | | | (_| |
 \___|_| |_|\__,_|

*/
