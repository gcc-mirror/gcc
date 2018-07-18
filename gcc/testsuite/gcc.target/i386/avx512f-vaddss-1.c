/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vaddss\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vaddss\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vaddss\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vaddss\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vaddss\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128 x1, x2, x3;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x1 = _mm_mask_add_ss (x1, m, x2, x3);
  x1 = _mm_maskz_add_ss (m, x1, x2);
  x1 = _mm_add_round_ss (x1, x2, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x1 = _mm_mask_add_round_ss (x1, m, x2, x3, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  x1 = _mm_maskz_add_round_ss (m, x1, x2, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}
