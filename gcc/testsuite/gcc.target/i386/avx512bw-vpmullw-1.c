/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpmullw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpmullw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmullw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmullw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmullw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmullw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmullw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x, y, z;
volatile __m256i xq, yq, zq;
volatile __m128i xw, yw, zw;

void extern
avx512bw_test (void)
{
  x = _mm512_mullo_epi16 (y, z);
  x = _mm512_mask_mullo_epi16 (x, 3, y, z);
  x = _mm512_maskz_mullo_epi16 (3, y, z);
  xq = _mm256_mask_mullo_epi16 (xq, 3, yq, zq);
  xq = _mm256_maskz_mullo_epi16 (3, yq, zq);
  xw = _mm_mask_mullo_epi16 (xw, 3, yw, zw);
  xw = _mm_maskz_mullo_epi16 (3, yw, zw);
}
