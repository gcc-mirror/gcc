/* { dg-do compile } */
/* { dg-options "-mavx512pf -O2" } */
/* { dg-final { scan-assembler-times "vgatherpf0dpd\[ \\t\]+\[^\{\n\]*\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>

volatile __m256i idx;
volatile __mmask8 m8;
void *base;

void extern
avx512pf_test (void)
{
  _mm512_prefetch_i32gather_pd (idx, base, 8, _MM_HINT_T0);
  _mm512_mask_prefetch_i32gather_pd (idx, m8, base, 8, _MM_HINT_T0);
}
