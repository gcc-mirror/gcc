/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vcvtusi2shq\[ \\t\]+\[^%\n\]*%r\[^\{\n\]*\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtusi2shq\[ \\t\]+\[^%\n\]*%r\[^\{\n\]*\{ru-sae\}\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128h x;
volatile unsigned long long n;

void extern
avx512f_test (void)
{
  x = _mm_cvtu64_sh (x, n);
  x = _mm_cvt_roundu64_sh (x, n, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
}
