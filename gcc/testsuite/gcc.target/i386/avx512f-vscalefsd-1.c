/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vscalefsd\[ \\t\]+\[^\n\]*%xmm\[0-9\]\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vscalefsd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%xmm\[0-9\]" 1 } } */

#include <immintrin.h>

volatile __m128d x;

void extern
avx512f_test (void)
{
  x = _mm_scalef_sd (x, x);
  x = _mm_scalef_round_sd (x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
}
