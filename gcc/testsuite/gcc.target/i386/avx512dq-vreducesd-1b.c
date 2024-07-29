/* { dg-do compile } */
/* { dg-options "-mavx512dq -O0" } */
/* { dg-final { scan-assembler-times "vreducesd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

#define IMM 123

volatile __m128d x1, x2, xx1, xx2;
volatile __mmask8 m;

void extern
avx512dq_test (void)
{
  xx1 = _mm_reduce_round_sd (xx1, xx2, IMM, _MM_FROUND_NO_EXC);
}
