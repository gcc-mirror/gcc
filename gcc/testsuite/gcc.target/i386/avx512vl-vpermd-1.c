/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpermd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i x;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  x = _mm256_permutexvar_epi32 (x, x);
  x = _mm256_maskz_permutexvar_epi32 (m, x, x);
  x = _mm256_mask_permutexvar_epi32 (x, m, x, x);
}
