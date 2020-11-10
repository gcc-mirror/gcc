/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vcvtsi2ssl\[ \\t\]+\[^%\n\]*%e\[^\{\n\]*\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtsi2ssl\[ \\t\]+\[^%\n\]*%e\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128 x, y;
volatile int n;

void extern
avx512f_test (void)
{
  x = _mm_cvt_roundi32_ss (x, n, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  y = _mm_cvti32_ss (x, n);
}
