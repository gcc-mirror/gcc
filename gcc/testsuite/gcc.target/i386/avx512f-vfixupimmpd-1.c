/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfixupimmpd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512d x1, x2;
volatile __m512i y;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x1 = _mm512_fixupimm_pd (x1, x2, y, 3);
  x1 = _mm512_mask_fixupimm_pd (x1, m, x2, y, 3);
  x1 = _mm512_maskz_fixupimm_pd (m, x1, x2, y, 3);
  x1 = _mm512_fixupimm_round_pd (x1, x2, y, 3, _MM_FROUND_NO_EXC);
  x1 = _mm512_mask_fixupimm_round_pd (x1, m, x2, y, 3, _MM_FROUND_NO_EXC);
  x1 = _mm512_maskz_fixupimm_round_pd (m, x1, x2, y, 3, _MM_FROUND_NO_EXC);
}
