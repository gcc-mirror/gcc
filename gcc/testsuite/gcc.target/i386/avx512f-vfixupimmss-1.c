/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vfixupimmss\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfixupimmss\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfixupimmss\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfixupimmss\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfixupimmss\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfixupimmss\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128 x;
volatile __m128i y;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm_fixupimm_ss (x, x, y, 3);
  x = _mm_mask_fixupimm_ss (x, m, x, y, 3);
  x = _mm_maskz_fixupimm_ss (m, x, x, y, 3);
  x = _mm_fixupimm_round_ss (x, x, y, 3, _MM_FROUND_NO_EXC);
  x = _mm_mask_fixupimm_round_ss (x, m, x, y, 3, _MM_FROUND_NO_EXC);
  x = _mm_maskz_fixupimm_round_ss (m, x, x, y, 3, _MM_FROUND_NO_EXC);
}
