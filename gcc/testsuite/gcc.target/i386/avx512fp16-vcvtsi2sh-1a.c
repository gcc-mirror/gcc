/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vcvtsi2shl\[ \\t\]+\[^%\n\]*%e\[^\{\n\]*\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtsi2shl\[ \\t\]+\[^%\n\]*%e\[^\{\n\]*\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128h x;
volatile int n;

void extern
avx512f_test (void)
{
  x = _mm_cvti32_sh (x, n);
  x = _mm_cvt_roundi32_sh (x, n, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
}
