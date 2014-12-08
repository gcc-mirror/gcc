/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpermw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpermw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpermw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpermw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x1;
volatile __m256i x2;
volatile __m128i x3;
volatile __mmask32 m1;
volatile __mmask16 m2;
volatile __mmask8 m3;

void extern
avx512bw_test (void)
{
  x1 = _mm512_permutexvar_epi16 (x1, x1);
  x1 = _mm512_maskz_permutexvar_epi16 (m1, x1, x1);
  x1 = _mm512_mask_permutexvar_epi16 (x1, m1, x1, x1);
  x2 = _mm256_permutexvar_epi16 (x2, x2);
  x2 = _mm256_maskz_permutexvar_epi16 (m2, x2, x2);
  x2 = _mm256_mask_permutexvar_epi16 (x2, m2, x2, x2);
  x3 = _mm_permutexvar_epi16 (x3, x3);
  x3 = _mm_maskz_permutexvar_epi16 (m3, x3, x3);
  x3 = _mm_mask_permutexvar_epi16 (x3, m3, x3, x3);
}
