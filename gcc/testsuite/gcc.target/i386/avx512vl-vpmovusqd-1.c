/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpmovusqd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovusqd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovusqd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}{z}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovusqd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovusqd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmovusqd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}{z}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128i x, z;
volatile __m256i y;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  z = _mm_cvtusepi64_epi32 (x);
  z = _mm_mask_cvtusepi64_epi32 (z, m, x);
  z = _mm_maskz_cvtusepi64_epi32 (m, x);
  z = _mm256_cvtusepi64_epi32 (y);
  z = _mm256_mask_cvtusepi64_epi32 (z, m, y);
  z = _mm256_maskz_cvtusepi64_epi32 (m, y);
}
