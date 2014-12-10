/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vfmadd...sd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128d a, b, c;

void extern
avx512f_test (void)
{
  a = _mm_fmadd_round_sd (a, b, c, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
}
