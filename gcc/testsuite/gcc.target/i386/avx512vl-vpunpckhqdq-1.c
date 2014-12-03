/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpunpckhqdq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpunpckhqdq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpunpckhqdq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpunpckhqdq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i x, y, z;
volatile __m128i a, b, c;
volatile __mmask8 m;

void extern
avx512bw_test (void)
{
  x = _mm256_mask_unpackhi_epi64 (x, m, y, z);
  x = _mm256_maskz_unpackhi_epi64 (m, y, z);
  a = _mm_mask_unpackhi_epi64 (a, m, b, c);
  a = _mm_maskz_unpackhi_epi64 (m, b, c);
}
