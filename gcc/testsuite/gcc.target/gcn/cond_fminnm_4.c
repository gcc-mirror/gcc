/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -dp" } */

#define FN(X) __builtin_fmin##X
#include "cond_fmaxnm_4.c"

/* { dg-final { scan-assembler-times {sminv64sf3} 3 } } */
/* { dg-final { scan-assembler-times {movv64sf_exec} 3 } } */
/* { dg-final { scan-assembler-times {sminv64sf3} 3 } } */
/* { dg-final { scan-assembler-times {movv64df_exec} 3 } } */

/* { dg-final { scan-assembler-not {\tv_writelane_b32\tv[0-9]+, vcc_..} } } */