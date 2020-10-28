/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vcvtsi2sdl\[ \\t\]+\[^%\n\]*%e\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128d x;
volatile int n;

void extern
avx512f_test (void)
{
  x = _mm_cvti32_sd (x, n);
}
