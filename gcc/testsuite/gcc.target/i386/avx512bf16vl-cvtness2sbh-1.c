/* { dg-do compile } */
/* { dg-options "-mavx512bf16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vcvtneps2bf16x\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __bfloat16 res;
volatile float x1;

void extern
avx512bf16_test (void)
{
  res = _mm_cvtness_sbh (x1);
}
