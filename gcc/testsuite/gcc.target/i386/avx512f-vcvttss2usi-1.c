/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vcvttss2usi\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+.{6}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttss2usi\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+.{6}(?:\n|\[ \\t\]+#)" 1 } } */
#include <immintrin.h>

volatile __m128 x;
volatile unsigned y;

void extern
avx512f_test (void)
{
  y = _mm_cvttss_u32 (x);
  y = _mm_cvtt_roundss_u32 (x, _MM_FROUND_NO_EXC);
}
