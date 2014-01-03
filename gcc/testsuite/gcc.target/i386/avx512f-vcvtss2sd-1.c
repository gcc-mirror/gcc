/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vcvtss2sd\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]\[^\n\]*%xmm\[0-9\]\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m128d s1, r;
volatile __m128 s2;

void extern
avx512f_test (void)
{
  r = _mm_cvt_roundss_sd (s1, s2, _MM_FROUND_NO_EXC);
}
