/* { dg-do compile } */
/* { dg-options "-mavx512dq -O2" } */
/* { dg-final { scan-assembler-times "vrangesd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangesd\[ \\t\]+\[^\$\n\]*\\$\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangesd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangesd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangesd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrangesd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */


#include <immintrin.h>

volatile __m128d x1, x2;
volatile __mmask8 m;

void extern
avx512dq_test (void)
{
  x1 = _mm_range_sd (x1, x2, 3);
  x1 = _mm_mask_range_sd (x1, m, x1, x2, 3);
  x1 = _mm_maskz_range_sd (m, x1, x2, 3);

  x1 = _mm_range_round_sd (x1, x2, 3, _MM_FROUND_NO_EXC);
  x1 = _mm_mask_range_round_sd (x1, m, x1, x2, 3, _MM_FROUND_NO_EXC);
  x1 = _mm_maskz_range_round_sd (m, x1, x2, 3, _MM_FROUND_NO_EXC);
}
