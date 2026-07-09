/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math -dp" } */
/* The 'scan-assembler' directives are specific to 64-lane vectors.
   { dg-additional-options --param=gcn-preferred-vectorization-factor=64 } */

#define FN(X) __builtin_fmin##X
#include "cond_fmaxnm_4.c"

/* { dg-final { scan-assembler-times {sminv64sf3} 3 } } */
/* { dg-final { scan-assembler-times {v_cndmask[^\n]*movv64sf} 3 } } */
/* { dg-final { scan-assembler-times {sminv64df3} 3 } } */
/* { dg-final { scan-assembler-times {v_mov[^\n]*movv64df[^\n]*exec} 3 } } */

/* { dg-final { scan-assembler-not {\tv_writelane_b32\tv[0-9]+, vcc_..} } } */
