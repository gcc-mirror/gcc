/* { dg-do compile } */
/* { dg-options "-mavx10v2aux -O2 -fno-fuse-ops-with-volatile-access" } */
/* { dg-final { scan-assembler-times "vcvtps2bf8x\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8x\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8x\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8y\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8y\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8y\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8z\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8z\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8z\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8sx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8sx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8sx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8sy\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8sy\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8sy\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8sz\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8sz\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2bf8sz\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8x\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8x\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8x\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8y\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8y\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8y\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8z\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8z\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8z\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8sx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8sx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8sx\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8sy\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8sy\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8sy\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8sz\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8sz\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2hf8sz\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128 a1;
volatile __m256 a2;
volatile __m512 a3;
volatile __m128i x128i;
volatile __mmask8 m8;
volatile __mmask16 m16;

void extern
avx10v2aux_vcvtps2bf8_test (void)
{
  x128i = _mm_cvtps_bf8 (a1);
  x128i = _mm_mask_cvtps_bf8 (x128i, m8, a1);
  x128i = _mm_maskz_cvtps_bf8 (m8, a1);

  x128i = _mm256_cvtps_bf8 (a2);
  x128i = _mm256_mask_cvtps_bf8 (x128i, m8, a2);
  x128i = _mm256_maskz_cvtps_bf8 (m8, a2);

  x128i = _mm512_cvtps_bf8 (a3);
  x128i = _mm512_mask_cvtps_bf8 (x128i, m16, a3);
  x128i = _mm512_maskz_cvtps_bf8 (m16, a3);
}

void extern
avx10v2aux_vcvtps2bf8s_test (void)
{
  x128i = _mm_cvts_ps_bf8 (a1);
  x128i = _mm_mask_cvts_ps_bf8 (x128i, m8, a1);
  x128i = _mm_maskz_cvts_ps_bf8 (m8, a1);

  x128i = _mm256_cvts_ps_bf8 (a2);
  x128i = _mm256_mask_cvts_ps_bf8 (x128i, m8, a2);
  x128i = _mm256_maskz_cvts_ps_bf8 (m8, a2);

  x128i = _mm512_cvts_ps_bf8 (a3);
  x128i = _mm512_mask_cvts_ps_bf8 (x128i, m16, a3);
  x128i = _mm512_maskz_cvts_ps_bf8 (m16, a3);
}

void extern
avx10v2aux_vcvtps2hf8_test (void)
{
  x128i = _mm_cvtps_hf8 (a1);
  x128i = _mm_mask_cvtps_hf8 (x128i, m8, a1);
  x128i = _mm_maskz_cvtps_hf8 (m8, a1);

  x128i = _mm256_cvtps_hf8 (a2);
  x128i = _mm256_mask_cvtps_hf8 (x128i, m8, a2);
  x128i = _mm256_maskz_cvtps_hf8 (m8, a2);

  x128i = _mm512_cvtps_hf8 (a3);
  x128i = _mm512_mask_cvtps_hf8 (x128i, m16, a3);
  x128i = _mm512_maskz_cvtps_hf8 (m16, a3);
}

void extern
avx10v2aux_vcvtps2hf8s_test (void)
{
  x128i = _mm_cvts_ps_hf8 (a1);
  x128i = _mm_mask_cvts_ps_hf8 (x128i, m8, a1);
  x128i = _mm_maskz_cvts_ps_hf8 (m8, a1);

  x128i = _mm256_cvts_ps_hf8 (a2);
  x128i = _mm256_mask_cvts_ps_hf8 (x128i, m8, a2);
  x128i = _mm256_maskz_cvts_ps_hf8 (m8, a2);

  x128i = _mm512_cvts_ps_hf8 (a3);
  x128i = _mm512_mask_cvts_ps_hf8 (x128i, m16, a3);
  x128i = _mm512_maskz_cvts_ps_hf8 (m16, a3);
}
