/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vcvttph2uw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vcvttph2uw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2uw\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2uw\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2uw\[ \\t\]+\{sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i res, res1, res2;
volatile __m512h x1, x2, x3;
volatile __mmask32 m32;

void extern
avx512f_test (void)
{
  res = _mm512_cvttph_epu16 (x1);
  res1 = _mm512_mask_cvttph_epu16 (res, m32, x2);
  res2 = _mm512_maskz_cvttph_epu16 (m32, x3);
  res = _mm512_cvtt_roundph_epu16 (x1, 4);
  res1 = _mm512_mask_cvtt_roundph_epu16 (res, m32, x2, 8);
  res2 = _mm512_maskz_cvtt_roundph_epu16 (m32, x3, 8);
}
