/* { dg-do compile } */
/* { dg-options "-mavx10v2aux -O2 -fno-fuse-ops-with-volatile-access" } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2bf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8s\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8s\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%ymm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtbiasps2hf8s\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%zmm\[0-9\]+,\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128 a1;
volatile __m256 a2;
volatile __m512 a3;
volatile __m128i b1;
volatile __m256i b2;
volatile __m512i b3;
volatile __m128i x128i;
volatile __mmask8 m8;
volatile __mmask16 m16;

void extern
avx10v2aux_vcvtbiasps2bf8_test (void)
{
  x128i = _mm_cvtbiasps_bf8 (b1, a1);
  x128i = _mm_mask_cvtbiasps_bf8 (x128i, m8, b1, a1);
  x128i = _mm_maskz_cvtbiasps_bf8 (m8, b1, a1);

  x128i = _mm256_cvtbiasps_bf8 (b2, a2);
  x128i = _mm256_mask_cvtbiasps_bf8 (x128i, m8, b2, a2);
  x128i = _mm256_maskz_cvtbiasps_bf8 (m8, b2, a2);

  x128i = _mm512_cvtbiasps_bf8 (b3, a3);
  x128i = _mm512_mask_cvtbiasps_bf8 (x128i, m16, b3, a3);
  x128i = _mm512_maskz_cvtbiasps_bf8 (m16, b3, a3);
}

void extern
avx10v2aux_vcvtbiasps2bf8s_test (void)
{
  x128i = _mm_cvts_biasps_bf8 (b1, a1);
  x128i = _mm_mask_cvts_biasps_bf8 (x128i, m8, b1, a1);
  x128i = _mm_maskz_cvts_biasps_bf8 (m8, b1, a1);

  x128i = _mm256_cvts_biasps_bf8 (b2, a2);
  x128i = _mm256_mask_cvts_biasps_bf8 (x128i, m8, b2, a2);
  x128i = _mm256_maskz_cvts_biasps_bf8 (m8, b2, a2);

  x128i = _mm512_cvts_biasps_bf8 (b3, a3);
  x128i = _mm512_mask_cvts_biasps_bf8 (x128i, m16, b3, a3);
  x128i = _mm512_maskz_cvts_biasps_bf8 (m16, b3, a3);
}

void extern
avx10v2aux_vcvtbiasps2hf8_test (void)
{
  x128i = _mm_cvtbiasps_hf8 (b1, a1);
  x128i = _mm_mask_cvtbiasps_hf8 (x128i, m8, b1, a1);
  x128i = _mm_maskz_cvtbiasps_hf8 (m8, b1, a1);

  x128i = _mm256_cvtbiasps_hf8 (b2, a2);
  x128i = _mm256_mask_cvtbiasps_hf8 (x128i, m8, b2, a2);
  x128i = _mm256_maskz_cvtbiasps_hf8 (m8, b2, a2);

  x128i = _mm512_cvtbiasps_hf8 (b3, a3);
  x128i = _mm512_mask_cvtbiasps_hf8 (x128i, m16, b3, a3);
  x128i = _mm512_maskz_cvtbiasps_hf8 (m16, b3, a3);
}

void extern
avx10v2aux_vcvtbiasps2hf8s_test (void)
{
  x128i = _mm_cvts_biasps_hf8 (b1, a1);
  x128i = _mm_mask_cvts_biasps_hf8 (x128i, m8, b1, a1);
  x128i = _mm_maskz_cvts_biasps_hf8 (m8, b1, a1);

  x128i = _mm256_cvts_biasps_hf8 (b2, a2);
  x128i = _mm256_mask_cvts_biasps_hf8 (x128i, m8, b2, a2);
  x128i = _mm256_maskz_cvts_biasps_hf8 (m8, b2, a2);

  x128i = _mm512_cvts_biasps_hf8 (b3, a3);
  x128i = _mm512_mask_cvts_biasps_hf8 (x128i, m16, b3, a3);
  x128i = _mm512_maskz_cvts_biasps_hf8 (m16, b3, a3);
}
