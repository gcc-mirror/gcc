/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vcvtusi2shl\[ \\t\]+\[^%\n\]*%e\[^\{\n\]*\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtusi2shl\[ \\t\]+\[^%\n\]*%e\[^\{\n\]*\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128h x;
volatile unsigned n;

void extern
avx512f_test (void)
{
  x = _mm_cvtu32_sh (x, n);
  x = _mm_cvt_roundu32_sh (x, n, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
}
