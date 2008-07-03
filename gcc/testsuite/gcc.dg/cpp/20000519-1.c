/* Regression test for preprocessor crash.
   Reported by Mathias Froehlich <frohlich@na.uni-tuebingen.de>.  */
/* { dg-do preprocess } */
/* { dg-options "-ansi" } */
#define foo

#define __CAT__(a,b,c,d) a##b##c##d
#define CAT(a,b,c,d) __CAT__(a,b,c,d)

#define bar CAT(,foo,bar,)
bar
