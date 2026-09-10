/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_double } */

#define FN(X) __builtin_fmin##X
#include "cond_fmaxnm_4_run.c"
