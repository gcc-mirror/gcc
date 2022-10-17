/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vrcpsh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrcpsh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vrcpsh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128h res, x1, x2;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
  res = _mm_rcp_sh (x1, x2);
  res = _mm_mask_rcp_sh (res, m8, x1, x2);
  res = _mm_maskz_rcp_sh (m8, x1, x2);
}
