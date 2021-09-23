/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vcmpph\[ \\t\]+\\\$1\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%k\[0-9\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpph\[ \\t\]+\\\$2\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%k\[0-9\]\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpph\[ \\t\]+\\\$3\[^\n\r]*\{sae\}\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%k\[0-9\]\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcmpph\[ \\t\]+\[^\{\n\]*\\\$4\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%k\[0-9\]\{%k\[0-9\]\}\[^\n\r]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __mmask32 res, res1, res2;
volatile __m512h x1, x2;
volatile __mmask32 m32;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
  res = _mm512_cmp_ph_mask (x1, x2, 1);
  res1 = _mm512_mask_cmp_ph_mask (m32, x1, x2, 2);
  res = _mm512_cmp_round_ph_mask (x1, x2, 3, 8);
  res1 = _mm512_mask_cmp_round_ph_mask (m32, x1, x2, 4, 4);
}
