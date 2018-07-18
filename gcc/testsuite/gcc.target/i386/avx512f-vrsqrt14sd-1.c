/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vrsqrt14sd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrsqrt14sd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrsqrt14sd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128d x1, x2, x3;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x1 = _mm_rsqrt14_sd (x1, x2);
  x1 = _mm_mask_rsqrt14_sd (x3, m, x1, x2);
  x1 = _mm_maskz_rsqrt14_sd (m, x1, x2);
}
