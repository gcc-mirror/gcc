/* { dg-do compile } */
/* { dg-options "-msse -O2" } */
/* { dg-final { scan-assembler-times "comiss\[ \\t\]+\[^\n\]*\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 6  } } */
/* { dg-final { scan-assembler-times "jp" 2 } } */
#include <xmmintrin.h>

volatile __m128 x1, x2;
volatile int res;

void extern
sse_comi_test (void)
{
  res = _mm_comieq_ss (x1, x2);
  res = _mm_comilt_ss (x1, x2);
  res = _mm_comile_ss (x1, x2);
  res = _mm_comigt_ss (x1, x2);
  res = _mm_comige_ss (x1, x2);
  res = _mm_comineq_ss (x1, x2);
}
