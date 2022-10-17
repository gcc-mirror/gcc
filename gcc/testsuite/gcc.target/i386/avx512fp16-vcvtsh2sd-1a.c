/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vcvtsh2sd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtsh2sd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtsh2sd\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtsh2sd\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtsh2sd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m128d res;
volatile __m128d x1;
volatile __m128h x2;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
  res = _mm_cvtsh_sd (x1, x2);
  res = _mm_mask_cvtsh_sd (res, m8, x1, x2);
  res = _mm_maskz_cvtsh_sd (m8, x1, x2);
  res = _mm_cvt_roundsh_sd (x1, x2, 8);
  res = _mm_mask_cvt_roundsh_sd (res, m8, x1, x2, 8);
  res = _mm_maskz_cvt_roundsh_sd (m8, x1, x2, 4);
}
