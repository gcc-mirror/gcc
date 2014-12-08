/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vcvttps2dq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttps2dq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttps2dq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttps2dq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256 s1;
volatile __m128 s2;
volatile __m256i res1;
volatile __m128i res2;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  res1 = _mm256_mask_cvttps_epi32 (res1, m, s1);
  res1 = _mm256_maskz_cvttps_epi32 (m, s1);
  res2 = _mm_mask_cvttps_epi32 (res2, m, s2);
  res2 = _mm_maskz_cvttps_epi32 (m, s2);
}
