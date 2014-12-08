/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vpcmpuq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[1-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpuq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[1-7\]\{%k\[0-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  m = _mm512_cmp_epu64_mask (x, x, _MM_CMPINT_LE);
  m = _mm512_mask_cmp_epu64_mask (m, x, x, _MM_CMPINT_UNUSED);
}
