/* { dg-do compile } */
/* { dg-options "-mavx512er -O2" } */
/* { dg-final { scan-assembler-times "vrcp28ss\[ \\t\]+\[^\n\]*%xmm\[0-9\]\[\\n\]" 2 } } */
/* { dg-final { scan-assembler-times "vrcp28ss\[ \\t\]+\[^\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]\[^\{\]*\n" 1 } } */

#include <immintrin.h>

volatile __m128 x, y;

void extern
avx512er_test (void)
{
  x = _mm_rcp28_ss (x, y);
  x = _mm_rcp28_round_ss (x, y, _MM_FROUND_NO_EXC);
}
