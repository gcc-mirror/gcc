/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vcvtpd2phy\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2phy\[ \\t\]+%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2phy\[ \\t\]+%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2phx\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2phx\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2phx\[ \\t\]+%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128h res3;
volatile __m256d x2;
volatile __m128d x3;
volatile __mmask16 m16;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
  res3 = _mm256_cvtpd_ph (x2);
  res3 = _mm256_mask_cvtpd_ph (res3, m16, x2);
  res3 = _mm256_maskz_cvtpd_ph (m16, x2);

  res3 = _mm_cvtpd_ph (x3);
  res3 = _mm_mask_cvtpd_ph (res3, m8, x3);
  res3 = _mm_maskz_cvtpd_ph (m8, x3);
}
