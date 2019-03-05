/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vptestmq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[0-7\](?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vptestmq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n^k\]*%k\[0-7\]\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512i x;
volatile __mmask8 m8;

void extern
avx512f_test (void)
{
  m8 = _mm512_test_epi64_mask (x, x);
  m8 = _mm512_mask_test_epi64_mask (3, x, x);
}
