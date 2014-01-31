/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vcvtsd2siq\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\n\]*%xmm\[0-9\]" 1 } } */

#include <immintrin.h>

volatile __m128d x;
volatile unsigned long long y;

void extern
avx512f_test (void)
{
  y = _mm_cvt_roundsd_i64 (x, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}
