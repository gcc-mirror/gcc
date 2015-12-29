/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vcvtsd2usi\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+.{6}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtsd2usi\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\{\n\]*%xmm\[0-9\]+.{6}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128d x;
volatile unsigned long long y;

void extern
avx512f_test (void)
{
  y = _mm_cvtsd_u64 (x);
  y = _mm_cvt_roundsd_u64 (x, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
}
