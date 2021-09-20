/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vfnmadd...sh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd...sh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vfnmadd231sh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfnmadd...sh\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vfnmadd...sh\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd...sh\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd231sh\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd...sh\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128h a, b, c;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  a = _mm_fnmadd_sh (a, b, c);
  a = _mm_mask_fnmadd_sh (a, m, b, c);
  c = _mm_mask3_fnmadd_sh (a, b, c, m);
  a = _mm_maskz_fnmadd_sh (m, a, b, c);
  a = _mm_fnmadd_round_sh (a, b, c, _MM_FROUND_TO_NEAREST_INT
			   | _MM_FROUND_NO_EXC);
  a = _mm_mask_fnmadd_round_sh (a, m, b, c, _MM_FROUND_TO_NEG_INF
				| _MM_FROUND_NO_EXC);
  c = _mm_mask3_fnmadd_round_sh (a, b, c, m, _MM_FROUND_TO_POS_INF
				 | _MM_FROUND_NO_EXC);
  a = _mm_maskz_fnmadd_round_sh (m, a, b, c, _MM_FROUND_TO_ZERO
				 | _MM_FROUND_NO_EXC);
}
