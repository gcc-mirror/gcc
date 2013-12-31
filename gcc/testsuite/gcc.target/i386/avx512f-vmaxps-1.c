/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vmaxps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\{\]" 6 } } */
/* { dg-final { scan-assembler-times "vmaxps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vmaxps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 2 } } */
/* { dg-final { scan-assembler-times "vmaxps\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\[^\{\]" 3 } } */
/* { dg-final { scan-assembler-times "vmaxps\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vmaxps\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512 x;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  x = _mm512_max_ps (x, x);
  x = _mm512_mask_max_ps (x, m, x, x);
  x = _mm512_maskz_max_ps (m, x, x);
  x = _mm512_max_round_ps (x, x, _MM_FROUND_NO_EXC);
  x = _mm512_mask_max_round_ps (x, m, x, x, _MM_FROUND_NO_EXC);
  x = _mm512_maskz_max_round_ps (m, x, x, _MM_FROUND_NO_EXC);
}
