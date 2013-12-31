/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler "vpcmpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n^k\]*%k\[1-7\]\[^\{\]" } } */
/* { dg-final { scan-assembler "vpcmpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n^k\]*%k\[1-7\]\{" } } */

#include <immintrin.h>

volatile __m512i x;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  m = _mm512_cmp_epi32_mask (x, x, _MM_CMPINT_GE);
  m = _mm512_mask_cmp_epi32_mask (m, x, x, _MM_CMPINT_NLE);
}
