/* { dg-do compile } */
/* { dg-options "-mavx10v2aux -O2 -fno-fuse-ops-with-volatile-access" } */
/* { dg-final { scan-assembler-times "vcvtbf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvthf82ps\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128i x128i;
volatile __m128 xf128;
volatile __m256 xf256;
volatile __m512 xf512;
volatile __mmask8 m8;
volatile __mmask16 m16;

void extern
avx10v2aux_vcvtbf82ps_test (void)
{
  xf128 = _mm_cvtbf8_ps (x128i);
  xf128 = _mm_mask_cvtbf8_ps (xf128, m8, x128i);
  xf128 = _mm_maskz_cvtbf8_ps (m8, x128i);

  xf256 = _mm256_cvtbf8_ps (x128i);
  xf256 = _mm256_mask_cvtbf8_ps (xf256, m8, x128i);
  xf256 = _mm256_maskz_cvtbf8_ps (m8, x128i);

  xf512 = _mm512_cvtbf8_ps (x128i);
  xf512 = _mm512_mask_cvtbf8_ps (xf512, m16, x128i);
  xf512 = _mm512_maskz_cvtbf8_ps (m16, x128i);
}

void extern
avx10v2aux_vcvthf82ps_test (void)
{
  xf128 = _mm_cvthf8_ps (x128i);
  xf128 = _mm_mask_cvthf8_ps (xf128, m8, x128i);
  xf128 = _mm_maskz_cvthf8_ps (m8, x128i);

  xf256 = _mm256_cvthf8_ps (x128i);
  xf256 = _mm256_mask_cvthf8_ps (xf256, m8, x128i);
  xf256 = _mm256_maskz_cvthf8_ps (m8, x128i);

  xf512 = _mm512_cvthf8_ps (x128i);
  xf512 = _mm512_mask_cvthf8_ps (xf512, m16, x128i);
  xf512 = _mm512_maskz_cvthf8_ps (m16, x128i);
}
