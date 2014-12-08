/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpbroadcastw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __m256i y;
volatile __m128i z;
volatile short w;
volatile __mmask32 mx;
volatile __mmask16 my;
volatile __mmask8 mz;

void extern
avx512bw_test (void)
{
  x = _mm512_broadcastw_epi16 (z);
  x = _mm512_mask_broadcastw_epi16 (x, mx, z);
  x = _mm512_maskz_broadcastw_epi16 (mx, z);
  y = _mm256_mask_broadcastw_epi16 (y, my, z);
  y = _mm256_maskz_broadcastw_epi16 (my, z);
  z = _mm_mask_broadcastw_epi16 (z, mz, z);
  z = _mm_maskz_broadcastw_epi16 (mz, z);

  x = _mm512_set1_epi16 (w);
  x = _mm512_mask_set1_epi16 (x, mx, w);
  x = _mm512_maskz_set1_epi16 (mx, w);
  y = _mm256_mask_set1_epi16 (y, my, w);
  y = _mm256_maskz_set1_epi16 (my, w);
  z = _mm_mask_set1_epi16 (z, mz, w);
  z = _mm_maskz_set1_epi16 (mz, w);
}
