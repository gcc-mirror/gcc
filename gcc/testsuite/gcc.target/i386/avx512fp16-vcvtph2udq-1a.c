/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vcvtph2udq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vcvtph2udq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2udq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2udq\[ \\t\]+\{rn-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2udq\[ \\t\]+\{rz-sae\}\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i res, res1, res2;
volatile __m256h x1, x2, x3;
volatile __mmask16 m16;

void extern
avx512f_test (void)
{
  res = _mm512_cvtph_epu32 (x1);
  res1 = _mm512_mask_cvtph_epu32 (res, m16, x2);
  res2 = _mm512_maskz_cvtph_epu32 (m16, x3);
  res = _mm512_cvt_roundph_epu32 (x1, 4);
  res1 = _mm512_mask_cvt_roundph_epu32 (res, m16, x2, 8);
  res2 = _mm512_maskz_cvt_roundph_epu32 (m16, x3, 11);
}
