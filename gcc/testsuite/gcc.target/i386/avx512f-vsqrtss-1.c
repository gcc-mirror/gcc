/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vsqrtss\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%xmm\[0-9\]" 1 } } */

#include <immintrin.h>

volatile __m128 x1, x2;

void extern
avx512f_test (void)
{
  x1 = _mm_sqrt_round_ss (x1, x2, _MM_FROUND_TO_NEAREST_INT);
}
