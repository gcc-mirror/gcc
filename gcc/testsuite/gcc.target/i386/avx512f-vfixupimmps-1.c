/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vfixupimmps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\{\]" 6 } } */
/* { dg-final { scan-assembler-times "vfixupimmps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vfixupimmps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 2 } } */
/* { dg-final { scan-assembler-times "vfixupimmps\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\[^\{\]" 3 } } */
/* { dg-final { scan-assembler-times "vfixupimmps\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vfixupimmps\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512 x1, x2;
volatile __m512i y;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  x1 = _mm512_fixupimm_ps (x1, x2, y, 3);
  x1 = _mm512_mask_fixupimm_ps (x1, m, x2, y, 3);
  x1 = _mm512_maskz_fixupimm_ps (m, x1, x2, y, 3);
  x1 = _mm512_fixupimm_round_ps (x1, x2, y, 3, _MM_FROUND_NO_EXC);
  x1 = _mm512_mask_fixupimm_round_ps (x1, m, x2, y, 3, _MM_FROUND_NO_EXC);
  x1 = _mm512_maskz_fixupimm_round_ps (m, x1, x2, y, 3, _MM_FROUND_NO_EXC);
}
