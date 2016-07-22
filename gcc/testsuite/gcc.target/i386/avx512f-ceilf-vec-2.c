/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize -mavx512f" } */

#include "avx512f-ceilf-vec-1.c"

/* { dg-final { scan-assembler-times "vrndscaleps\[^\n\]+zmm\[0-9\](?:\n|\[ \\t\]+#)" 1 } } */
