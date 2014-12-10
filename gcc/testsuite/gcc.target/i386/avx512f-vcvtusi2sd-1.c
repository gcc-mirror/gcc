/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vcvtusi2sd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128d x;
volatile unsigned n;

void extern
avx512f_test (void)
{
  x = _mm_cvtu32_sd (x, n);
}
