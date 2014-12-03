/* { dg-do compile } */
/* { dg-options "-mavx512vbmi -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpermt2b\[ \\t\]+\[^\n\]*%zmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vpermt2b\[ \\t\]+\[^\n\]*%ymm\[0-9\]+" 3 } } *
/* { dg-final { scan-assembler-times "vpermt2b\[ \\t\]+\[^\n\]*%xmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vpermt2b\[ \\t\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpermt2b\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpermt2b\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vpermt2b\[ \\t\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vpermt2b\[ \\t\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vpermt2b\[ \\t\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}" 1 } } */

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
  x3 = _mm512_permutex2var_epi8 (x3, z, x3);
  x3 = _mm512_mask_permutex2var_epi8 (x3, m3, z, x3);
  x3 = _mm512_maskz_permutex2var_epi8 (m3, x3, z, x3);
  x2 = _mm256_permutex2var_epi8 (x2, y, x2);
  x2 = _mm256_mask_permutex2var_epi8 (x2, m2, y, x2);
  x2 = _mm256_maskz_permutex2var_epi8 (m2, x2, y, x2);
  x1 = _mm_permutex2var_epi8 (x1, x, x1);
  x1 = _mm_mask_permutex2var_epi8 (x1, m1, x, x1);
  x1 = _mm_maskz_permutex2var_epi8 (m1, x1, x, x1);
}
