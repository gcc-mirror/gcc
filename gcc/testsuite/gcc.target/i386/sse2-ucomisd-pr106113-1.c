/* { dg-do compile } */
/* { dg-options "-msse2 -O2" } */
/* { dg-final { scan-assembler-times "ucomisd\[ \\t\]+\[^\n\]*\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 6  } } */
/* { dg-final { scan-assembler-times "jp" 2 } } */
#include <xmmintrin.h>

volatile __m128d x1, x2;
volatile int res;

void extern
sse2_ucomisd_test (void)
{
  res = _mm_ucomieq_sd (x1, x2);
  res = _mm_ucomilt_sd (x1, x2);
  res = _mm_ucomile_sd (x1, x2);
  res = _mm_ucomigt_sd (x1, x2);
  res = _mm_ucomige_sd (x1, x2);
  res = _mm_ucomineq_sd (x1, x2);
}
