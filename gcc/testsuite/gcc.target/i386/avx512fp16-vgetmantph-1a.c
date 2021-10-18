/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */
/* { dg-final { scan-assembler-times "vgetmantph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgetmantph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512h x, y;
volatile __mmask32 m;

void extern
avx512f_test (void)
{
  x = _mm512_getmant_ph (y, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src);
  x = _mm512_mask_getmant_ph (x, m, y, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src);
  x = _mm512_maskz_getmant_ph (m, y, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src);
  x = _mm512_getmant_round_ph (y, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src, _MM_FROUND_NO_EXC);
  x = _mm512_mask_getmant_round_ph (x, m, y, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src, _MM_FROUND_NO_EXC);
  x = _mm512_maskz_getmant_round_ph (m, y, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src, _MM_FROUND_NO_EXC);
}
