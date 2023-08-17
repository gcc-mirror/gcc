/* { dg-do compile } */
/* { dg-options "-mavx10.1 -O2" } */
/* { dg-final { scan-assembler-times "vextractf64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+.{7}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vextractf64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+.{7}\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vextractf64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+.{7}\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

volatile __m256d x;
volatile __m128d y;

void extern
avx10_1_test (void)
{
  y = _mm256_extractf64x2_pd (x, 1);
  y = _mm256_mask_extractf64x2_pd (y, 2, x, 1);
  y = _mm256_maskz_extractf64x2_pd (2, x, 1);
}
