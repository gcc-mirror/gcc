/* { dg-do compile } */
/* { dg-options "-mavx512bf16 -O2" } */
/* { dg-final { scan-assembler-times "vcvtne2ps2bf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtne2ps2bf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vcvtne2ps2bf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512bh res;
volatile __m512 x1, x2;
volatile __mmask32 m32;

void extern
avx512bf16_test (void)
{
  res = _mm512_cvtne2ps_pbh (x1, x2);
  res = _mm512_mask_cvtne2ps_pbh (res, m32, x1, x2);
  res = _mm512_maskz_cvtne2ps_pbh (m32, x1, x2);
}
