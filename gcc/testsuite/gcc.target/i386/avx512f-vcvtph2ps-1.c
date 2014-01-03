/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vcvtph2ps\[ \\t\]+\[^\n\]*%zmm\[0-9\]" 6 } } */
/* { dg-final { scan-assembler-times "vcvtph2ps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vcvtph2ps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 2 } } */
/* { dg-final { scan-assembler-times "vcvtph2ps\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]" 3 } } */
/* { dg-final { scan-assembler-times "vcvtph2ps\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2ps\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m256i x;
volatile __m512 y;

void extern
avx512f_test (void)
{
  y = _mm512_cvtph_ps (x);
  y = _mm512_mask_cvtph_ps (y, 4, x);
  y = _mm512_maskz_cvtph_ps (6, x);
  y = _mm512_cvt_roundph_ps (x, _MM_FROUND_NO_EXC);
  y = _mm512_mask_cvt_roundph_ps (y, 4, x, _MM_FROUND_NO_EXC);
  y = _mm512_maskz_cvt_roundph_ps (6, x, _MM_FROUND_NO_EXC);
}
