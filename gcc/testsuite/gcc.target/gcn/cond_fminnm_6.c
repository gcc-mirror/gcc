/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -dp" } */

#define FN(X) __builtin_fmin##X
#include "cond_fmaxnm_2.c"

/* { dg-final { scan-assembler-times {sminv64sf3_exec} 3 } } */
/* { dg-final { scan-assembler-times {sminv64df3_exec} 3 } } */

/* { dg-final { scan-assembler-not {\tv_writelane_b32\tv[0-9]+, vcc_..} } } */