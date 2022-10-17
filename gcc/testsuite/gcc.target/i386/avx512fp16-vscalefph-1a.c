/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vscalefph\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vscalefph\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vscalefph\[ \\t\]+%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vscalefph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vscalefph\[ \\t\]+\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vscalefph\[ \\t\]+\{rz-sae\}\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512h res, res1, res2;
volatile __m512h x1, x2;
volatile __mmask32 m32;

void extern
avx512f_test (void)
{
  res = _mm512_scalef_ph (x1, x2);
  res1 = _mm512_mask_scalef_ph (res1, m32, x1, x2);
  res2 = _mm512_maskz_scalef_ph (m32, x1, x2);
  res = _mm512_scalef_round_ph (x1, x2, 8);
  res1 = _mm512_mask_scalef_round_ph (res1, m32, x1, x2, 8);
  res2 = _mm512_maskz_scalef_round_ph (m32, x1, x2, 11);
}
