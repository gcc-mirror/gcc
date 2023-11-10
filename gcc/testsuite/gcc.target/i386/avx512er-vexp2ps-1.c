/* { dg-do compile } */
/* { dg-options "-mavx512er -O2" } */
/* { dg-warning "AVX512ER support will be removed in GCC 15" "" { target *-*-* } 0 } */
/* { dg-final { scan-assembler-times "vexp2ps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexp2ps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexp2ps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexp2ps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\[^\{\]*(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vexp2ps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vexp2ps\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512 x;
volatile __mmask16 m;

void extern
avx512er_test (void)
{
  x = _mm512_exp2a23_ps (x);
  x = _mm512_mask_exp2a23_ps (x, m, x);
  x = _mm512_maskz_exp2a23_ps (m, x);
  x = _mm512_exp2a23_round_ps (x, _MM_FROUND_NO_EXC);
  x = _mm512_mask_exp2a23_round_ps (x, m, x, _MM_FROUND_NO_EXC);
  x = _mm512_maskz_exp2a23_round_ps (m, x, _MM_FROUND_NO_EXC);
}
