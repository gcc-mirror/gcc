/* { dg-do compile } */
/* { dg-options "-mavx512bf16 -O2" } */
/* { dg-final { scan-assembler-times "vdpbf16ps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vdpbf16ps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vdpbf16ps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512 res;
volatile __m512bh x1, x2;
volatile __mmask16 m16;

void extern
avx512bf16_test (void)
{
  res = _mm512_dpbf16_ps (res, x1, x2);
  res = _mm512_mask_dpbf16_ps (res, m16, x1, x2);
  res = _mm512_maskz_dpbf16_ps (m16, res, x1, x2);
}
