/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vcvtsh2si\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%e\[ad]x" 1 } } */
/* { dg-final { scan-assembler-times "vcvtsh2si\[ \\t\]+\{rn-sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%e\[ad]x" 1 } } */


#include <immintrin.h>

volatile __m128h x1;
volatile int res1;

void extern
avx512f_test (void)
{
  res1 = _mm_cvtsh_i32 (x1);
  res1 = _mm_cvt_roundsh_i32 (x1, 8);
}
