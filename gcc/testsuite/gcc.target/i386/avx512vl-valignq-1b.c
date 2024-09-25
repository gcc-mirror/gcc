/* { dg-do compile } */
/* { dg-options "-O0 -mavx512vl" } */
/* { dg-final { scan-assembler-times "valignq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m256i y;
volatile __m128i x;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  x = _mm_mask_alignr_epi64 (x, m, x, x, 1);
}
