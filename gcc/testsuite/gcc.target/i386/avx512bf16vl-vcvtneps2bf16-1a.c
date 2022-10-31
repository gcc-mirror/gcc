/* { dg-do compile } */
/* { dg-options "-mavx512bf16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vcvtneps2bf16y\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneps2bf16y\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneps2bf16y\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneps2bf16x\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneps2bf16x\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneps2bf16x\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128bh res1, res2;
volatile __m128 x1;
volatile __m256 x2;
volatile __mmask8 m8;

void extern
avx512bf16_test (void)
{
  res2 = _mm256_cvtneps_pbh (x2);
  res2 = _mm256_mask_cvtneps_pbh (res2, m8, x2);
  res2 = _mm256_maskz_cvtneps_pbh (m8, x2);

  res1 = _mm_cvtneps_pbh (x1);
  res1 = _mm_mask_cvtneps_pbh (res1, m8, x1);
  res1 = _mm_maskz_cvtneps_pbh (m8, x1);
}
