/* { dg-do compile } */
/* { dg-options "-mavx512dq -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vextracti64x2\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+.{7}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vextracti64x2\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+.{7}\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vextracti64x2\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+.{7}\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vextracti64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+.{7}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vextracti64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+.{7}\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vextracti64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+.{7}\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

volatile __m512i x1;
volatile __m256i x2;
volatile __m128i y;

void extern
avx512dq_test (void)
{
  y = _mm512_extracti64x2_epi64 (x1, 3);
  y = _mm512_mask_extracti64x2_epi64 (y, 2, x1, 3);
  y = _mm512_maskz_extracti64x2_epi64 (2, x1, 3);
  y = _mm256_extracti64x2_epi64 (x2, 1);
  y = _mm256_mask_extracti64x2_epi64 (y, 2, x2, 1);
  y = _mm256_maskz_extracti64x2_epi64 (2, x2, 1);
}
