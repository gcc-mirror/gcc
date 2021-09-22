/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vcvtuqq2phz\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vcvtuqq2phz\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtuqq2phz\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtuqq2ph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtuqq2ph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128h res, res1, res2;
volatile __m512i x1, x2, x3;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
  res = _mm512_cvtepu64_ph (x1);
  res1 = _mm512_mask_cvtepu64_ph (res, m8, x2);
  res2 = _mm512_maskz_cvtepu64_ph (m8, x3);
  res = _mm512_cvt_roundepu64_ph (x1, 4);
  res1 = _mm512_mask_cvt_roundepu64_ph (res, m8, x2, 8);
  res2 = _mm512_maskz_cvt_roundepu64_ph (m8, x3, 11);
}
