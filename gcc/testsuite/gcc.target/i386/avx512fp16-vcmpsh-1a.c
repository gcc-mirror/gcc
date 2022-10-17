/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$3\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\[^\{\n\]*\\\$4\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\\\$3\[^\n\r]*\{sae\}\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpsh\[ \\t\]+\[^\{\n\]*\\\$4\[^\n\r]*\{sae\}\[^\n\r\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%k\[0-9\]\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __mmask8 res, res1, res2;
volatile __m128h x1, x2;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
  res = _mm_cmp_sh_mask (x1, x2, 3);
  res = _mm_mask_cmp_sh_mask (m8, x1, x2, 4);
  res = _mm_cmp_round_sh_mask (x1, x2, 3, 8);
  res1 = _mm_mask_cmp_round_sh_mask (m8, x1, x2, 4, 8);
}
