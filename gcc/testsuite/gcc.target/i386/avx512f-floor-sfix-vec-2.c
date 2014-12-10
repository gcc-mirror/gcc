/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -mavx512f" } */

#include "avx512f-floor-sfix-vec-1.c"

/* { dg-final { scan-assembler-times "vrndscalepd\[^\n\]*zmm\[0-9\](?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vcvttpd2dq\[^\n\]*zmm\[0-9\].{7}(?:\n|\[ \\t\]+#)" 2 } } */
