/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vscalefsd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vscalefsd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vscalefsd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vscalefsd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */


#include <immintrin.h>

volatile __m128d x;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm_scalef_sd (x, x);
  x = _mm_scalef_round_sd (x, x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x = _mm_mask_scalef_round_sd (x, m, x, x, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  x = _mm_maskz_scalef_round_sd (m, x, x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}
