/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vsubsd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vsubsd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vsubsd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vsubsd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vsubsd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128d x1, x2, x3;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x1 = _mm_mask_sub_sd (x1, m, x2, x3);
  x1 = _mm_maskz_sub_sd (m, x1, x2);
  x1 = _mm_sub_round_sd (x1, x2, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x1 = _mm_mask_sub_round_sd (x1, m, x2, x3, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  x1 = _mm_maskz_sub_round_sd (m, x1, x2, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}
