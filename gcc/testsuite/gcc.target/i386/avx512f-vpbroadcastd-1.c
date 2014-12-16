/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+%(?:e|r\[0-9\]+d)\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+%(?:e|r\[0-9\]+d)\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+%(?:e|r\[0-9\]+d)\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __m128i y;
volatile int z;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  x = _mm512_broadcastd_epi32 (y);
  x = _mm512_mask_broadcastd_epi32 (x, m, y);
  x = _mm512_maskz_broadcastd_epi32 (m, y);

  x = _mm512_set1_epi32 (z);
  x = _mm512_mask_set1_epi32 (x, m, z);
  x = _mm512_maskz_set1_epi32 (m, z);
}
