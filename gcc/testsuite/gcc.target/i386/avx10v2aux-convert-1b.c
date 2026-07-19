/* { dg-do compile } */
/* { dg-options "-mavx10v2aux -O2 -fno-fuse-ops-with-volatile-access" } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8x\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8x\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8x\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8y\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8y\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8y\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8z\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8z\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8z\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8sx\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8sx\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8sx\[ \\t\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8sy\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8sy\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8sy\[ \\t\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8sz\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8sz\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtrops2hf8sz\[ \\t\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128 a1;
volatile __m256 a2;
volatile __m512 a3;
volatile __m128i x128i;
volatile __mmask8 m8;
volatile __mmask16 m16;

void extern
avx10v2aux_vcvtrops2hf8_test (void)
{
  x128i = _mm_cvtrops_hf8 (a1);
  x128i = _mm_mask_cvtrops_hf8 (x128i, m8, a1);
  x128i = _mm_maskz_cvtrops_hf8 (m8, a1);

  x128i = _mm256_cvtrops_hf8 (a2);
  x128i = _mm256_mask_cvtrops_hf8 (x128i, m8, a2);
  x128i = _mm256_maskz_cvtrops_hf8 (m8, a2);

  x128i = _mm512_cvtrops_hf8 (a3);
  x128i = _mm512_mask_cvtrops_hf8 (x128i, m16, a3);
  x128i = _mm512_maskz_cvtrops_hf8 (m16, a3);
}

void extern
avx10v2aux_vcvtrops2hf8s_test (void)
{
  x128i = _mm_cvts_rops_hf8 (a1);
  x128i = _mm_mask_cvts_rops_hf8 (x128i, m8, a1);
  x128i = _mm_maskz_cvts_rops_hf8 (m8, a1);

  x128i = _mm256_cvts_rops_hf8 (a2);
  x128i = _mm256_mask_cvts_rops_hf8 (x128i, m8, a2);
  x128i = _mm256_maskz_cvts_rops_hf8 (m8, a2);

  x128i = _mm512_cvts_rops_hf8 (a3);
  x128i = _mm512_mask_cvts_rops_hf8 (x128i, m16, a3);
  x128i = _mm512_maskz_cvts_rops_hf8 (m16, a3);
}
