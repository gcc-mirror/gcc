/* { dg-do compile } */
/* { dg-options "-mavx512dq -O2" } */
/* { dg-final { scan-assembler-times "vrangess\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangess\[ \\t\]+\[^\$\n\]*\\$\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangess\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangess\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangess\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangess\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128 x1, x2;
volatile __mmask8 m;

void extern
avx512dq_test (void)
{
  x1 = _mm_range_ss (x1, x2, 1);
  x1 = _mm_mask_range_ss (x1, m, x1, x2, 1);
  x1 = _mm_maskz_range_ss (m, x1, x2, 1);

  x1 = _mm_range_round_ss (x1, x2, 1, _MM_FROUND_NO_EXC);
  x1 = _mm_mask_range_round_ss (x1, m, x1, x2, 1, _MM_FROUND_NO_EXC);
  x1 = _mm_maskz_range_round_ss (m, x1, x2, 1, _MM_FROUND_NO_EXC);
}
