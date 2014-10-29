/* { dg-do compile } */
/* { dg-options "-mavx512dq -O2" } */
/* { dg-final { scan-assembler "kmovb\[ \\t\]+\[^\n\]*%k\[1-7\]" } } */

#include <immintrin.h>

volatile __m512d x;
volatile __mmask8 m;

void extern
avx512dq_test (void)
{
  x = _mm512_mask_add_pd (x, m, x, x);
}
