/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vscalefpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\{\]" 6 } } */
/* { dg-final { scan-assembler-times "vscalefpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vscalefpd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 2 } } */
/* { dg-final { scan-assembler-times "vscalefpd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%zmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vscalefpd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vscalefpd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512d x;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x = _mm512_scalef_pd (x, x);
  x = _mm512_mask_scalef_pd (x, m, x, x);
  x = _mm512_maskz_scalef_pd (m, x, x);
  x = _mm512_scalef_round_pd (x, x, _MM_FROUND_TO_NEAREST_INT);
  x = _mm512_mask_scalef_round_pd (x, m, x, x, _MM_FROUND_TO_NEG_INF);
  x = _mm512_maskz_scalef_round_pd (m, x, x, _MM_FROUND_TO_ZERO);
}
