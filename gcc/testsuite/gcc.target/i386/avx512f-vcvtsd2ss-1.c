/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vcvtsd2ss\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%xmm\[0-9\]\[^\n\]*%xmm\[0-9\]\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m128 s1, r;
volatile __m128d s2;

void extern
avx512f_test (void)
{
  r = _mm_cvt_roundsd_ss (s1, s2, _MM_FROUND_TO_NEAREST_INT);
}
