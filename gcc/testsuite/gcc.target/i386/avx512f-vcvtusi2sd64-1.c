/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vcvtusi2sd\[ \\t\]+\[^\n\]*%xmm\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "vcvtusi2sd\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\n\]*%xmm\[0-9\]" 1 } } */

#include <immintrin.h>

volatile __m128d x;
volatile unsigned long long n;

void extern
avx512f_test (void)
{
  x = _mm_cvtu64_sd (x, n);
  x = _mm_cvt_roundu64_sd (x, n, _MM_FROUND_TO_POS_INF);
}
