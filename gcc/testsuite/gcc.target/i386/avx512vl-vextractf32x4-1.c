/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vextractf(?:128|32x4)\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vextractf32x4\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vextractf32x4\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256 x;
volatile __m128 y;

void extern
avx512vl_test (void)
{
  y = _mm256_extractf32x4_ps (x, 1);
  y = _mm256_mask_extractf32x4_ps (y, 2, x, 1);
  y = _mm256_maskz_extractf32x4_ps (2, x, 1);
}
