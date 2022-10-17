/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vcvtph2psx\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vcvtph2psx\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2psx\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2psx\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2psx\[ \\t\]+\{sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512 res, res1, res2;
volatile __m256h x1, x2, x3;
volatile __mmask16 m16;

void extern
avx512f_test (void)
{
  res = _mm512_cvtxph_ps (x1);
  res1 = _mm512_mask_cvtxph_ps (res, m16, x2);
  res2 = _mm512_maskz_cvtxph_ps (m16, x3);
  res = _mm512_cvtx_roundph_ps (x1, 4);
  res1 = _mm512_mask_cvtx_roundph_ps (res, m16, x2, 8);
  res2 = _mm512_maskz_cvtx_roundph_ps (m16, x3, 8);
}
