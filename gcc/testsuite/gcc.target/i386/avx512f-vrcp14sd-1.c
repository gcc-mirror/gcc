/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vrcp14sd\[ \\t\]+\[^\n\]*%xmm\[0-9\]\[^\{\]" 1 } } */

#include <immintrin.h>

volatile __m128d x1, x2;

void extern
avx512f_test (void)
{
  x1 = _mm_rcp14_sd (x1, x2);
}
