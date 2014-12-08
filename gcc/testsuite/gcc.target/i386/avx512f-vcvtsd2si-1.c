/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vcvtsd2si\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%xmm\[0-9\]+.{6}(?:\n|\[ \\t\]+#)" 1 } } */
#include <immintrin.h>

volatile __m128d x;
volatile unsigned y;

void extern
avx512f_test (void)
{
  y = _mm_cvt_roundsd_i32 (x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
}
