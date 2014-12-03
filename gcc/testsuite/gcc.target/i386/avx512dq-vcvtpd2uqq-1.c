/* { dg-do compile } */
/* { dg-options "-mavx512dq -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2uqq\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512d s1;
volatile __m256d s2;
volatile __m128d s3;
volatile __m512i res1;
volatile __m256i res2;
volatile __m128i res3;
volatile __mmask8 m;

void extern
avx512dq_test (void)
{
  res1 = _mm512_cvtpd_epu64 (s1);
  res2 = _mm256_cvtpd_epu64 (s2);
  res3 = _mm_cvtpd_epu64 (s3);

  res1 = _mm512_mask_cvtpd_epu64 (res1, m, s1);
  res2 = _mm256_mask_cvtpd_epu64 (res2, m, s2);
  res3 = _mm_mask_cvtpd_epu64 (res3, m, s3);

  res1 = _mm512_maskz_cvtpd_epu64 (m, s1);
  res2 = _mm256_maskz_cvtpd_epu64 (m, s2);
  res3 = _mm_maskz_cvtpd_epu64 (m, s3);

  res1 = _mm512_cvt_roundpd_epu64 (s1, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  res1 = _mm512_mask_cvt_roundpd_epu64 (res1, m, s1, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  res1 = _mm512_maskz_cvt_roundpd_epu64 (m, s1, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}
