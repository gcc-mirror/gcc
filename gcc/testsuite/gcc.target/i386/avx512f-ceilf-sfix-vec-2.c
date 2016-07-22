/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -mavx512f" } */

#include "avx512f-ceilf-sfix-vec-1.c"

/* { dg-final { scan-assembler-times "vrndscaleps\[^\n\]+zmm\[0-9\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttps2dq\[^\n\]+zmm\[0-9\].{7}(?:\n|\[ \\t\]+#)" 1 } } */
