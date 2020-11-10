/* { dg-do compile } */
/* { dg-options "-mavx512dq -O2" } */
/* { dg-final { scan-assembler-times "vreduceps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vreduceps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vreduceps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

#define IMM 123

volatile __m512 xx1;
volatile __mmask16 m16;

void extern
avx512dq_test (void)
{
  xx1 = _mm512_reduce_round_ps (xx1, IMM, _MM_FROUND_NO_EXC);

  xx1 = _mm512_mask_reduce_round_ps (xx1, m16, xx1, IMM, _MM_FROUND_NO_EXC);

  xx1 = _mm512_maskz_reduce_round_ps (m16, xx1, IMM, _MM_FROUND_NO_EXC);
}
