/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vfcmaddcsh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfcmaddcsh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfcmaddcsh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfcmaddcsh\[ \\t\]+\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfcmaddcsh\[ \\t\]+\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfcmaddcsh\[ \\t\]+\{rz-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovss\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vmovss\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}+(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m128h res, res1, res2;
volatile __m128h x1, x2, x3;
volatile __mmask8 m8;

void extern
avx128f_test (void)
{
  res = _mm_fcmadd_sch (x1, x2, x3);
  res1 = _mm_mask_fcmadd_sch (res1, m8, x1, x2);
  res1 = _mm_mask3_fcmadd_sch (res1, x1, x2, m8);
  res2 = _mm_maskz_fcmadd_sch (m8, x1, x2, x3);
  res = _mm_fcmadd_round_sch (x1, x2, x3, 8);
  res1 = _mm_mask_fcmadd_round_sch (res1, m8, x1, x2, 8);
  res1 = _mm_mask3_fcmadd_round_sch (res1, x1, x2, m8, 8);
  res2 = _mm_maskz_fcmadd_round_sch (m8, x1, x2, x3, 11);
}
