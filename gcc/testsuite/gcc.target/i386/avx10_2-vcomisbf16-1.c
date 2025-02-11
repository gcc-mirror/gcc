/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2-256 -O2" } */
/* { dg-final { scan-assembler-times "vcomisbf16\[ \\t\]+\[^{}\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 6 } } */
/* { dg-final { scan-assembler-times "jp" 2 } } */
#include <immintrin.h>

volatile __m128bh x1, x2;
volatile int res;

void extern
avx10_2_vcomi_test (void)
{
  res = _mm_comieq_sbh (x1, x2);
  res = _mm_comilt_sbh (x1, x2);
  res = _mm_comile_sbh (x1, x2);
  res = _mm_comigt_sbh (x1, x2);
  res = _mm_comige_sbh (x1, x2);
  res = _mm_comineq_sbh (x1, x2);
}
