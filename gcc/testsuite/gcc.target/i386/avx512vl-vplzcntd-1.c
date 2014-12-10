/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512cd -O2" } */
/* { dg-final { scan-assembler-times "vplzcntd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vplzcntd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vplzcntd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vplzcntd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vplzcntd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vplzcntd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i s;
volatile __m256i res;
volatile __m128i s2;
volatile __m128i res2;

void extern
avx512vl_test (void)
{
  res = _mm256_lzcnt_epi32 (s);
  res = _mm256_mask_lzcnt_epi32 (res, 2, s);
  res = _mm256_maskz_lzcnt_epi32 (2, s);
  res2 = _mm_lzcnt_epi32 (s2);
  res2 = _mm_mask_lzcnt_epi32 (res2, 2, s2);
  res2 = _mm_maskz_lzcnt_epi32 (2, s2);
}
