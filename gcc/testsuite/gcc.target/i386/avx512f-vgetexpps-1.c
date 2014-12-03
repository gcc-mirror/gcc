/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vgetexpps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1} } */

#include <immintrin.h>

volatile __m512 x;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  x = _mm512_getexp_ps (x);
  x = _mm512_mask_getexp_ps (x, m, x);
  x = _mm512_maskz_getexp_ps (m, x);
  x = _mm512_getexp_round_ps (x, _MM_FROUND_NO_EXC);
  x = _mm512_mask_getexp_round_ps (x, m, x, _MM_FROUND_NO_EXC);
  x = _mm512_maskz_getexp_round_ps (m, x, _MM_FROUND_NO_EXC);
}
