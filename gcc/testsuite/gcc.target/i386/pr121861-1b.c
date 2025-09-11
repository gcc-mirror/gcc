/* { dg-do compile } */
/* { dg-options "-Os -mfpmath=sse -mno-avx512f -mavx" } */

#include "pr121861-1a.c"

/* { dg-final { scan-assembler-times "vroundss" 3 } } */
/* { dg-final { scan-assembler-times "vroundsd" 3 } } */
