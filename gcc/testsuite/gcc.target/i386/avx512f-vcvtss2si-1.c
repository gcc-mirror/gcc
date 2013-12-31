/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vcvtss2si\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%xmm\[0-9\]" 1 } } */
#include <immintrin.h>

volatile __m128 x;
volatile unsigned y;

void extern
avx512f_test (void)
{
  y = _mm_cvt_roundss_i32 (x, _MM_FROUND_TO_NEAREST_INT);
}
