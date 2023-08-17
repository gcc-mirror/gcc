/* { dg-do compile } */
/* { dg-options "-mavx10.1 -O2" } */
/* { dg-final { scan-assembler-times "vextracti64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+.{7}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vextracti64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+.{7}\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vextracti64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+.{7}\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

volatile __m256i x;
volatile __m128i y;

void extern
avx10_1_test (void)
{
  y = _mm256_extracti64x2_epi64 (x, 1);
  y = _mm256_mask_extracti64x2_epi64 (y, 2, x, 1);
  y = _mm256_maskz_extracti64x2_epi64 (2, x, 1);
}

