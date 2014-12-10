/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpbroadcastb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __m256i y;
volatile __m128i z;
volatile char w;
volatile __mmask64 mx;
volatile __mmask32 my;
volatile __mmask16 mz;

void extern
avx512bw_test (void)
{
  x = _mm512_broadcastb_epi8 (z);
  x = _mm512_mask_broadcastb_epi8 (x, mx, z);
  x = _mm512_maskz_broadcastb_epi8 (mx, z);
  y = _mm256_mask_broadcastb_epi8 (y, my, z);
  y = _mm256_maskz_broadcastb_epi8 (my, z);
  z = _mm_mask_broadcastb_epi8 (z, mz, z);
  z = _mm_maskz_broadcastb_epi8 (mz, z);

  x = _mm512_set1_epi8 (w);
  x = _mm512_mask_set1_epi8 (x, mx, w);
  x = _mm512_maskz_set1_epi8 (mx, w);
  y = _mm256_mask_set1_epi8 (y, my, w);
  y = _mm256_maskz_set1_epi8 (my, w);
  z = _mm_mask_set1_epi8 (z, mz, w);
  z = _mm_maskz_set1_epi8 (mz, w);
}
