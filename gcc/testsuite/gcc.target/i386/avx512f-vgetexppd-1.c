/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vgetexppd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\{\]" 6} } */
/* { dg-final { scan-assembler-times "vgetexppd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 2} } */
/* { dg-final { scan-assembler-times "vgetexppd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 2} } */
/* { dg-final { scan-assembler-times "vgetexppd\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\[^\{\]" 3} } */
/* { dg-final { scan-assembler-times "vgetexppd\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1} } */
/* { dg-final { scan-assembler-times "vgetexppd\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1} } */

#include <immintrin.h>

volatile __m512d x;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm512_getexp_pd (x);
  x = _mm512_mask_getexp_pd (x, m, x);
  x = _mm512_maskz_getexp_pd (m, x);
  x = _mm512_getexp_round_pd (x, _MM_FROUND_NO_EXC);
  x = _mm512_mask_getexp_round_pd (x, m, x, _MM_FROUND_NO_EXC);
  x = _mm512_maskz_getexp_round_pd (m, x, _MM_FROUND_NO_EXC);
}
