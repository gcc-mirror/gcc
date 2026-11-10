/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */

#define FN(X) __builtin_fmin##X
#include "cond_fmaxnm_1_run.c"
