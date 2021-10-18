/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vgetexpph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1} } */
/* { dg-final { scan-assembler-times "vgetexpph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1} } */

#include <immintrin.h>

volatile __m512h x;
volatile __mmask32 m;

void extern
avx512f_test (void)
{
  x = _mm512_getexp_ph (x);
  x = _mm512_mask_getexp_ph (x, m, x);
  x = _mm512_maskz_getexp_ph (m, x);
  x = _mm512_getexp_round_ph (x, _MM_FROUND_NO_EXC);
  x = _mm512_mask_getexp_round_ph (x, m, x, _MM_FROUND_NO_EXC);
  x = _mm512_maskz_getexp_round_ph (m, x, _MM_FROUND_NO_EXC);
}
