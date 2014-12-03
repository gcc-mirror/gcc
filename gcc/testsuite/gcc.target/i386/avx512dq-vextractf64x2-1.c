/* { dg-do compile } */
/* { dg-options "-mavx512dq -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vextractf64x2\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+.{7}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vextractf64x2\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+.{7}\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vextractf64x2\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+.{7}\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vextractf64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+.{7}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vextractf64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+.{7}\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vextractf64x2\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+.{7}\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

volatile __m512d x1;
volatile __m256d x2;
volatile __m128d y;

void extern
avx512dq_test (void)
{
  y = _mm512_extractf64x2_pd (x1, 3);
  y = _mm512_mask_extractf64x2_pd (y, 2, x1, 3);
  y = _mm512_maskz_extractf64x2_pd (2, x1, 3);
  y = _mm256_extractf64x2_pd (x2, 1);
  y = _mm256_mask_extractf64x2_pd (y, 2, x2, 1);
  y = _mm256_maskz_extractf64x2_pd (2, x2, 1);
}
