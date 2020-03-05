/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-additional-options "-fno-common" { target *-*-darwin* } } */
/* { dg-final { scan-assembler-times "vpmovswb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovswb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovswb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}{z}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovswb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*res1\[^\n\]*\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpmovswb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovswb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovswb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}{z}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovswb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*res2\[^\n\]*\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpmovswb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovswb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovswb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}{z}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovswb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*res3\[^\n\]*\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

volatile __m128i x, z, res1;
volatile __m256i y, res2;
volatile __m512i u, res3;
volatile __mmask8 m1;
volatile __mmask16 m2;
volatile __mmask32 m3;

void extern
avx512bw_test (void)
{
  z = _mm_cvtsepi16_epi8 (x);
  z = _mm_mask_cvtsepi16_epi8 (z, m1, x);
  z = _mm_maskz_cvtsepi16_epi8 (m1, x);
  _mm_mask_cvtsepi16_storeu_epi8 ((void *) &res1, m1, x);
  z = _mm256_cvtsepi16_epi8 (y);
  z = _mm256_mask_cvtsepi16_epi8 (z, m2, y);
  z = _mm256_maskz_cvtsepi16_epi8 (m2, y);
  _mm256_mask_cvtsepi16_storeu_epi8 ((void *) &res2, m2, y);
  y = _mm512_cvtsepi16_epi8 (u);
  y = _mm512_mask_cvtsepi16_epi8 (y, m3, u);
  y = _mm512_maskz_cvtsepi16_epi8 (m3, u);
  _mm512_mask_cvtsepi16_storeu_epi8 ((void *) &res3, m3, u);
}
