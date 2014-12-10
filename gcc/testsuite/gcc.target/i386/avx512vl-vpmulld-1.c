/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpmulld\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmulld\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmulld\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}{z}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmulld\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}{z}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i y;
volatile __m128i z;
volatile __mmask8 myz;

void extern
avx512vl_test (void)
{
  y = _mm256_mask_mullo_epi32 (y, myz, y, y);
  y = _mm256_maskz_mullo_epi32 (myz, y, y);
  z = _mm_mask_mullo_epi32 (z, myz, z, z);
  z = _mm_maskz_mullo_epi32 (myz, z, z);
}
