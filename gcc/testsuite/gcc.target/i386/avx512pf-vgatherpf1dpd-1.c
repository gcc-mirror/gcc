/* { dg-do compile } */
/* { dg-options "-mavx512pf -O2" } */
/* { dg-final { scan-assembler-times "vgatherpf1dpd\[ \\t\]+\[^\n\]*\{%k\[1-7\]" 1 } } */

#include <immintrin.h>

volatile __m256i idx;
volatile __mmask8 m8;
int *base;

void extern
avx512pf_test (void)
{
  _mm512_mask_prefetch_i32gather_pd (idx, m8, base, 8, 1);
}
