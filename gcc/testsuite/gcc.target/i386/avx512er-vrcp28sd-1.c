/* { dg-do compile } */
/* { dg-options "-mavx512er -O2" } */
/* { dg-final { scan-assembler-times "vrcp28sd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vrcp28sd\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+\[^\{\]*(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128d x, y;

void extern
avx512er_test (void)
{
  x = _mm_rcp28_sd (x, y);
  x = _mm_rcp28_round_sd (x, y, _MM_FROUND_NO_EXC);
}
