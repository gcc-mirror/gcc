/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vcvtsi2ssq\[ \\t\]+\[^%\n\]*%r\[^\{\n\]*\{rz-sae\}\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128 x;
volatile long long n;

void extern
avx512f_test (void)
{
  x = _mm_cvt_roundi64_ss (x, n, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}
