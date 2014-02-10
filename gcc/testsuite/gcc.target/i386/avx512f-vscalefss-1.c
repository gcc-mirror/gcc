/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vscalefss\[ \\t\]+\[^\n\]*%xmm\[0-9\]\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vscalefss\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%xmm\[0-9\]" 1 } } */

#include <immintrin.h>

volatile __m128 x;

void extern
avx512f_test (void)
{
  x = _mm_scalef_ss (x, x);
  x = _mm_scalef_round_ss (x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
}
