/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpunpcklwd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpunpcklwd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpunpcklwd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpunpcklwd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpunpcklwd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpunpcklwd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpunpcklwd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i d, e, f;
volatile __m256i x, y, z;
volatile __m128i a, b, c;
volatile __mmask32 m1;
volatile __mmask16 m2;
volatile __mmask8 m3;

void extern
avx512bw_test (void)
{
  d = _mm512_unpacklo_epi16 (e, f);
  d = _mm512_mask_unpacklo_epi16 (d, m1, e, f);
  d = _mm512_maskz_unpacklo_epi16 (m1, e, f);
  x = _mm256_mask_unpacklo_epi16 (x, m2, y, z);
  x = _mm256_maskz_unpacklo_epi16 (m2, y, z);
  a = _mm_mask_unpacklo_epi16 (a, m3, b, c);
  a = _mm_maskz_unpacklo_epi16 (m3, b, c);
}
