/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vfnmsub...ph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfnmsub...ph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vfnmsub231ph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfnmsub...ph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfnmsub...ph\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub...ph\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub231ph\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub...ph\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m512h x1, x2, x3;
volatile __mmask32 m;

void extern
avx512f_test (void)
{
  x1 = _mm512_fnmsub_ph (x1, x2, x3);
  x1 = _mm512_mask_fnmsub_ph (x1, m, x2, x3);
  x3 = _mm512_mask3_fnmsub_ph (x1, x2, x3, m);
  x1 = _mm512_maskz_fnmsub_ph (m, x1, x2, x3);
  x1 = _mm512_fnmsub_round_ph (x1, x2, x3, _MM_FROUND_TO_NEAREST_INT
			       | _MM_FROUND_NO_EXC);
  x1 = _mm512_mask_fnmsub_round_ph (x1, m, x2, x3, _MM_FROUND_TO_NEG_INF
				    | _MM_FROUND_NO_EXC);
  x3 = _mm512_mask3_fnmsub_round_ph (x1, x2, x3, m, _MM_FROUND_TO_POS_INF
				     | _MM_FROUND_NO_EXC);
  x1 = _mm512_maskz_fnmsub_round_ph (m, x1, x2, x3, _MM_FROUND_TO_ZERO
				     | _MM_FROUND_NO_EXC);
}
