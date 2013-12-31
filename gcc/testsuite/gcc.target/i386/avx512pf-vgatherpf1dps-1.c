/* { dg-do compile } */
/* { dg-options "-mavx512pf -O2" } */
/* { dg-final { scan-assembler-times "vgatherpf1dps\[ \\t\]+\[^\n\]*\{%k\[1-7\]" 1 } } */

#include <immintrin.h>

volatile __m512i idx;
volatile __mmask16 m16;
int *base;

void extern
avx512pf_test (void)
{
  _mm512_mask_prefetch_i32gather_ps (idx, m16, base, 8, 1);
}
