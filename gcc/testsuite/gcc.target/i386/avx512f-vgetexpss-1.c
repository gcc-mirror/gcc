/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vgetexpss\[ \\t\]+\[^\n\]*%xmm\[0-9\]\, %xmm\[0-9\]\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vgetexpss\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]\, %xmm\[0-9\]\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m128 x;

void extern
avx512f_test (void)
{
  x = _mm_getexp_ss (x, x);
  x = _mm_getexp_round_ss (x, x, _MM_FROUND_NO_EXC);
}
