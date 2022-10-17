/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vscalefsh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vscalefsh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vscalefsh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vscalefsh\[ \\t\]+\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vscalefsh\[ \\t\]+\{rz-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128h res, x1, x2;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
  res = _mm_scalef_sh (x1, x2);
  res = _mm_mask_scalef_sh (res, m8, x1, x2);
  res = _mm_maskz_scalef_sh (m8, x1, x2);
  res = _mm_scalef_round_sh (x1, x2, 4);
  res = _mm_mask_scalef_round_sh (res, m8, x1, x2, 8);
  res = _mm_maskz_scalef_round_sh (m8, x1, x2, 11);
}
