/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vcvtps2udq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2udq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2udq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2udq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2udq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2udq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256 x1;
volatile __m128 x2;
volatile __m256i z1;
volatile __m128i z2;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  z1 = _mm256_cvtps_epu32 (x1);
  z1 = _mm256_mask_cvtps_epu32 (z1, m, x1);
  z1 = _mm256_maskz_cvtps_epu32 (m, x1);
  z2 = _mm_cvtps_epu32 (x2);
  z2 = _mm_mask_cvtps_epu32 (z2, m, x2);
  z2 = _mm_maskz_cvtps_epu32 (m, x2);
}
