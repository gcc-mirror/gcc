/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler "vpcmpq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n^k\]*%k\[1-7\]\{" } } */
/* { dg-final { scan-assembler "vpcmpq\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n^k\]*%k\[1-7\]\[^\{\]" } } */

#include <immintrin.h>

volatile __m512i x;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  m = _mm512_cmp_epi64_mask (x, x, _MM_CMPINT_NE);
  m = _mm512_mask_cmp_epi64_mask (m, x, x, _MM_CMPINT_NLT);
}
