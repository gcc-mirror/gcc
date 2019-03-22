/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vfnmsub...sd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vfnmsub231sd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfnmsub...sd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfnmsub...sd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub...sd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub231sd\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub...sd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128d a, b, c;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  a = _mm_mask_fnmsub_sd (a, m, b, c);
  c = _mm_mask3_fnmsub_sd (a, b, c, m);
  a = _mm_maskz_fnmsub_sd (m, a, b, c);
  a = _mm_fnmsub_round_sd (a, b, c, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  a = _mm_mask_fnmsub_round_sd (a, m, b, c, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  c = _mm_mask3_fnmsub_round_sd (a, b, c, m, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  a = _mm_maskz_fnmsub_round_sd (m, a, b, c, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}
