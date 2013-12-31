/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -mavx512f" } */

#include "avx512f-floor-sfix-vec-1.c"

/* { dg-final { scan-assembler "vrndscalepd\[^\n\]*zmm\[0-9\]" } } */
/* { dg-final { scan-assembler "vcvttpd2dq\[^\n\]*zmm\[0-9\]" } } */
