/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpavgb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpavgb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpavgb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpavgb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpavgb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpavgb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpavgb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128i x;
volatile __m256i y;
volatile __m512i z;
volatile __mmask64 m1;
volatile __mmask32 m2;
volatile __mmask16 m3;

void extern
avx512bw_test (void)
{
  z = _mm512_avg_epu8 (z, z);
  z = _mm512_mask_avg_epu8 (z, m1, z, z);
  z = _mm512_maskz_avg_epu8 (m1, z, z);
  y = _mm256_mask_avg_epu8 (y, m2, y, y);
  y = _mm256_maskz_avg_epu8 (m2, y, y);
  x = _mm_mask_avg_epu8 (x, m3, x, x);
  x = _mm_maskz_avg_epu8 (m3, x, x);
}
