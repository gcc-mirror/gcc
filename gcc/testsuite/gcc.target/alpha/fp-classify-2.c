/* With -mieee the comparisons carry the software-completion suffix and are
   usable, so the generic expansions are kept.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mieee" } */

int isnan_d (double x) { return __builtin_isnan (x); }

/* { dg-final { scan-assembler "cmptun/su" } } */
