/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -mavx512f" } */
/* { dg-require-effective-target c99_runtime } */

#include "avx512f-rint-sfix-vec-1.c"

/* { dg-final { scan-assembler-times "vcvtpd2dq\[^\n\]+ymm\[0-9\](?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vinserti64x4\[^\n\]+zmm\[0-9\](?:\n|\[ \\t\]+#)" 1 } } */
