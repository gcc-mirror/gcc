/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vfcmulcsh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfcmulcsh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\{\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfcmulcsh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfcmulcsh\[ \\t\]+\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfcmulcsh\[ \\t\]+\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-times "vfcmulcsh\[ \\t\]+\{rz-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[0-9\]\}\{z\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m128h res, res1, res2;
volatile __m128h x1, x2, x3;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
  res = _mm_fcmul_sch (x1, x2);
  res1 = _mm_mask_fcmul_sch (res1, m8, x1, x2);
  res2 = _mm_maskz_fcmul_sch (m8, x1, x2);
  res = _mm_fcmul_round_sch (x1, x2, 8);
  res1 = _mm_mask_fcmul_round_sch (res1, m8, x1, x2, 8);
  res2 = _mm_maskz_fcmul_round_sch (m8, x1, x2, 11);
  
  res = _mm_cmul_sch (x1, x2);
  res1 = _mm_mask_cmul_sch (res1, m8, x1, x2);
  res2 = _mm_maskz_cmul_sch (m8, x1, x2);
  res = _mm_cmul_round_sch (x1, x2, 8);
  res1 = _mm_mask_cmul_round_sch (res1, m8, x1, x2, 8);
  res2 = _mm_maskz_cmul_round_sch (m8, x1, x2, 11);
}
