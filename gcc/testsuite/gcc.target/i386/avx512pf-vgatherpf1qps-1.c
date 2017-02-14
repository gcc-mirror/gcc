/* { dg-do compile } */
/* { dg-options "-mavx512pf -O2" } */
/* { dg-final { scan-assembler-times "vgatherpf1qps\[ \\t\]+\[^\{\n\]*\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m512i idx;
volatile __mmask8 m8;
int *base;

void extern
avx512pf_test (void)
{
  _mm512_prefetch_i64gather_ps (idx, base, 8, _MM_HINT_T1);
  _mm512_mask_prefetch_i64gather_ps (idx, m8, base, 8, _MM_HINT_T1);
}
