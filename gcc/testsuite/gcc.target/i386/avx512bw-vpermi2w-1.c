/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpermi2w\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermi2w\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpermi2w\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x3;
volatile __m256i x2;
volatile __m128i x1;
volatile __m512i z;
volatile __m256i y;
volatile __m128i x;
volatile __mmask32 m3;
volatile __mmask16 m2;
volatile __mmask8 m1;

void extern
avx512bw_test (void)
{
  x3 = _mm512_mask2_permutex2var_epi16 (x3, z, m3, x3);
  x2 = _mm256_mask2_permutex2var_epi16 (x2, y, m2, x2);
  x1 = _mm_mask2_permutex2var_epi16 (x1, x, m1, x1);
}
