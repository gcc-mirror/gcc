/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vfcmaddcph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfcmaddcph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfcmaddcph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfcmaddcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfcmaddcph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfcmaddcph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "(?:vblendmps\[ \\t\]+%zmm\[0-9\]+|vmovaps\[ \\t\]+)\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m512h res, res1, res2;
volatile __m512h x1, x2, x3;
volatile __mmask16 m16;

void extern
avx512f_test (void)
{
  res = _mm512_fcmadd_pch (x1, x2, x3);
  res1 = _mm512_mask_fcmadd_pch (res1, m16, x1, x2);
  res1 = _mm512_mask3_fcmadd_pch (res1, x1, x2, m16);
  res2 = _mm512_maskz_fcmadd_pch (m16, x1, x2, x3);
  res = _mm512_fcmadd_round_pch (x1, x2, x3, 8);
  res1 = _mm512_mask_fcmadd_round_pch (res1, m16, x1, x2, 8);
  res1 = _mm512_mask3_fcmadd_round_pch (res1, x1, x2, m16, 8);
  res2 = _mm512_maskz_fcmadd_round_pch (m16, x1, x2, x3, 11);
}
