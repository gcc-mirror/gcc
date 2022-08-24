/* { dg-do compile } */
/* { dg-options "-msse2 -O2" } */
/* { dg-final { scan-assembler-times "comisd\[ \\t\]+\[^\n\]*\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 6  } } */
/* { dg-final { scan-assembler-times "jp" 2 } } */
#include <xmmintrin.h>

volatile __m128d x1, x2;
volatile int res;

void extern
sse2_comisd_test (void)
{
  res = _mm_comieq_sd (x1, x2);
  res = _mm_comilt_sd (x1, x2);
  res = _mm_comile_sd (x1, x2);
  res = _mm_comigt_sd (x1, x2);
  res = _mm_comige_sd (x1, x2);
  res = _mm_comineq_sd (x1, x2);
}
