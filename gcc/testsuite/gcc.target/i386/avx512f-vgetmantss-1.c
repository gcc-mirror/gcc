/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vgetmantss\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vgetmantss\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128 x, y, z;

void extern
avx512f_test (void)
{
  x = _mm_getmant_ss (y, z, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src);
  x = _mm_getmant_round_ss (y, z, _MM_MANT_NORM_p75_1p5, _MM_MANT_SIGN_src,
		      _MM_FROUND_NO_EXC);
}
