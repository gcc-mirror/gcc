/* { dg-do compile } */
/* { dg-options "-msse -O2" } */
/* { dg-final { scan-assembler-times "ucomiss\[ \\t\]+\[^\n\]*\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 6  } } */
/* { dg-final { scan-assembler-times "jp" 2 } } */
#include <xmmintrin.h>

volatile __m128 x1, x2;
volatile int res;

void extern
sse_ucomi_test (void)
{
  res = _mm_ucomieq_ss (x1, x2);
  res = _mm_ucomilt_ss (x1, x2);
  res = _mm_ucomile_ss (x1, x2);
  res = _mm_ucomigt_ss (x1, x2);
  res = _mm_ucomige_ss (x1, x2);
  res = _mm_ucomineq_ss (x1, x2);
}
