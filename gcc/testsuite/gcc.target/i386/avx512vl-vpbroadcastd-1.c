/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+%(?:e|r\[0-9\]+d)\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+%(?:e|r\[0-9\]+d)\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+%(?:e|r\[0-9\]+d)\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+%(?:e|r\[0-9\]+d)\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 { target { ! ia32 } } } } */

#include <immintrin.h>

volatile __m256i x;
volatile __m128i y;
volatile int z;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  x = _mm256_mask_broadcastd_epi32 (x, m, y);
  x = _mm256_maskz_broadcastd_epi32 (m, y);
  y = _mm_mask_broadcastd_epi32 (y, m, y);
  y = _mm_maskz_broadcastd_epi32 (m, y);

  x = _mm256_mask_set1_epi32 (x, m, z);
  x = _mm256_maskz_set1_epi32 (m, z);
  y = _mm_mask_set1_epi32 (y, m, z);
  y = _mm_maskz_set1_epi32 (m, z);
}
