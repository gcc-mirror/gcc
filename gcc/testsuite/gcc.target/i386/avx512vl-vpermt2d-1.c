/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vperm\[ti]2d\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vperm\[ti]2d\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vperm\[ti]2d\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vperm\[ti]2d\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vperm\[ti]2d\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vperm\[ti]2d\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i x1;
volatile __m128i x2;
volatile __m256i y;
volatile __m128i z;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  x1 = _mm256_permutex2var_epi32 (x1, y, x1);
  x1 = _mm256_mask_permutex2var_epi32 (x1, m, y, x1);
  x1 = _mm256_maskz_permutex2var_epi32 (m, x1, y, x1);
  x2 = _mm_permutex2var_epi32 (x2, z, x2);
  x2 = _mm_mask_permutex2var_epi32 (x2, m, z, x2);
  x2 = _mm_maskz_permutex2var_epi32 (m, x2, z, x2);
}
