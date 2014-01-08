/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vfixupimmsd\[ \\t\]+\[^\n\]*%xmm\[0-9\]\[^\{\]" 6 } } */
/* { dg-final { scan-assembler-times "vfixupimmsd\[ \\t\]+\[^\n\]*%xmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vfixupimmsd\[ \\t\]+\[^\n\]*%xmm\[0-9\]\{%k\[1-7\]\}\{z\}" 2 } } */
/* { dg-final { scan-assembler-times "vfixupimmsd\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]\[^\{\]" 3 } } */
/* { dg-final { scan-assembler-times "vfixupimmsd\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vfixupimmsd\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m128d x;
volatile __m128i y;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm_fixupimm_sd (x, x, y, 3);
  x = _mm_mask_fixupimm_sd (x, m, x, y, 3);
  x = _mm_maskz_fixupimm_sd (m, x, x, y, 3);
  x = _mm_fixupimm_round_sd (x, x, y, 3, _MM_FROUND_NO_EXC);
  x = _mm_mask_fixupimm_round_sd (x, m, x, y, 3, _MM_FROUND_NO_EXC);
  x = _mm_maskz_fixupimm_round_sd (m, x, x, y, 3, _MM_FROUND_NO_EXC);
}
