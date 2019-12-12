/* { dg-do compile } */
/* { dg-options "-mavx512bf16 -O2" } */
/* { dg-final { scan-assembler-times "vcvtneps2bf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtneps2bf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vcvtneps2bf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256bh res;
volatile __m512 x1;
volatile __mmask16 m16;

void extern
avx512bf16_test (void)
{
  res = _mm512_cvtneps_pbh (x1);
  res = _mm512_mask_cvtneps_pbh (res, m16, x1);
  res = _mm512_maskz_cvtneps_pbh (m16, x1);
}
