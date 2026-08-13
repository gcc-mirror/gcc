/* { dg-do compile } */
/* { dg-options "-mavx10v2aux -O2 -fno-fuse-ops-with-volatile-access" } */
/* { dg-final { scan-assembler-times "vpmovssdb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovssdb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovssdb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovssdb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovssdb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovssdb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovssdb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovssdb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovssdb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovssdb\[ \\t\]+%xmm\[0-9\]+, \\(\[^\{\n\]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovssdb\[ \\t\]+%ymm\[0-9\]+, \\(\[^\{\n\]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovssdb\[ \\t\]+%zmm\[0-9\]+, \\(\[^\{\n\]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128i x128i;
volatile __m256i x256i;
volatile __m512i x512i;
volatile __m128i res128i;
volatile __mmask8 m8;
volatile __mmask16 m16;
char *p;

void extern
avx10v2aux_vpmovssdb_test (void)
{
  res128i = _mm_cvtss_epi32_epi8 (x128i);
  res128i = _mm_mask_cvtss_epi32_epi8 (res128i, m8, x128i);
  res128i = _mm_maskz_cvtss_epi32_epi8 (m8, x128i);
  _mm_mask_cvtss_epi32_storeu_epi8 ((void *) p, m8, x128i);

  res128i = _mm256_cvtss_epi32_epi8 (x256i);
  res128i = _mm256_mask_cvtss_epi32_epi8 (res128i, m8, x256i);
  res128i = _mm256_maskz_cvtss_epi32_epi8 (m8, x256i);
  _mm256_mask_cvtss_epi32_storeu_epi8 ((void *) p, m8, x256i);

  res128i = _mm512_cvtss_epi32_epi8 (x512i);
  res128i = _mm512_mask_cvtss_epi32_epi8 (res128i, m16, x512i);
  res128i = _mm512_maskz_cvtss_epi32_epi8 (m16, x512i);
  _mm512_mask_cvtss_epi32_storeu_epi8 ((void *) p, m16, x512i);
}
