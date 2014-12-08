/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpmulhuw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpmulhuw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmulhuw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmulhuw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmulhuw\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmulhuw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpmulhuw\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x, y, z;
volatile __m256i xq, yq, zq;
volatile __m128i xw, yw, zw;

void extern
avx512bw_test (void)
{
  x = _mm512_mulhi_epu16 (y, z);
  x = _mm512_mask_mulhi_epu16 (x, 2, y, z);
  x = _mm512_maskz_mulhi_epu16 (2, y, z);
  xq = _mm256_mask_mulhi_epu16 (xq, 2, yq, zq);
  xq = _mm256_maskz_mulhi_epu16 (2, yq, zq);
  xw = _mm_mask_mulhi_epu16 (xw, 2, yw, zw);
  xw = _mm_maskz_mulhi_epu16 (2, yw, zw);
}
