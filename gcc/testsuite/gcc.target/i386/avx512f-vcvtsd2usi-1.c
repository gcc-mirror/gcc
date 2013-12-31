/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vcvtsd2usi\[ \\t\]+\[^\n\]*%xmm\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "vcvtsd2usi\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\n\]*%xmm\[0-9\]" 1 } } */
#include <immintrin.h>

volatile __m128d x;
volatile unsigned y;

void extern
avx512f_test (void)
{
  y = _mm_cvtsd_u32 (x);
  y = _mm_cvt_roundsd_u32 (x, _MM_FROUND_TO_NEG_INF);
}
