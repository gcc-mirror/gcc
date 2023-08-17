/* { dg-do compile } */
/* { dg-options "-mavx10.1 -O2" } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256d s1;
volatile __m128d s2;
volatile __m256i res1;
volatile __m128i res2;
volatile __mmask8 m;

void extern
avx10_1_test (void)
{
  res1 = _mm256_cvtpd_epu64 (s1);
  res2 = _mm_cvtpd_epu64 (s2);

  res1 = _mm256_mask_cvtpd_epu64 (res1, m, s1);
  res2 = _mm_mask_cvtpd_epu64 (res2, m, s2);

  res1 = _mm256_maskz_cvtpd_epu64 (m, s1);
  res2 = _mm_maskz_cvtpd_epu64 (m, s2);
}
