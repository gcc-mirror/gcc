/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vcvtss2sil?\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+.{6}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtss2sil?\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+.{6}(?:\n|\[ \\t\]+#)" 1 } } */
#include <immintrin.h>

volatile __m128 x;
volatile unsigned y, z;

void extern
avx512f_test (void)
{
  y = _mm_cvt_roundss_i32 (x, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  z = _mm_cvtss_i32 (x);
}
