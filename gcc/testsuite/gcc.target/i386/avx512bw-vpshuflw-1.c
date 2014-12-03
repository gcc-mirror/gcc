/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpshuflw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpshuflw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshuflw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshuflw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshuflw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshuflw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpshuflw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i z;
volatile __m256i y;
volatile __m128i x;
volatile __mmask32 m1;
volatile __mmask16 m2;
volatile __mmask8 m3;

void extern
avx512bw_test (void)
{
  z = _mm512_shufflelo_epi16 (z, _MM_PERM_AADB);
  z = _mm512_mask_shufflelo_epi16 (z, m1, z, _MM_PERM_AADB);
  z = _mm512_maskz_shufflelo_epi16 (m1, z, _MM_PERM_AADB);
  y = _mm256_mask_shufflelo_epi16 (y, m2, y, _MM_PERM_AADB);
  y = _mm256_maskz_shufflelo_epi16 (m2, y, _MM_PERM_AADB);
  x = _mm_mask_shufflelo_epi16 (x, m3, x, _MM_PERM_AADB);
  x = _mm_maskz_shufflelo_epi16 (m3, x, _MM_PERM_AADB);
}
