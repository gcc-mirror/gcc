/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpalignr\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpalignr\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpalignr\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpalignr\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpalignr\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpalignr\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpalignr\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i z;
volatile __m256i y;
volatile __m128i x;
volatile __mmask64 m1;
volatile __mmask32 m2;
volatile __mmask16 m3;

void extern
avx512bw_test (void)
{
  z = _mm512_alignr_epi8 (z, z, 10);
  z = _mm512_mask_alignr_epi8 (z, m1, z, z, 10);
  z = _mm512_maskz_alignr_epi8 (m1, z, z, 10);
  y = _mm256_mask_alignr_epi8 (y, m2, y, y, 10);
  y = _mm256_maskz_alignr_epi8 (m2, y, y, 10);
  x = _mm_mask_alignr_epi8 (x, m3, x, x, 10);
  x = _mm_maskz_alignr_epi8 (m3, x, x, 10);
}
