/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vcvttsh2usi\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%eax" 1 } } */
/* { dg-final { scan-assembler-times "vcvttsh2usi\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%eax" 1 } } */

#include <immintrin.h>

volatile __m128h x1;
volatile unsigned int res1;

void extern
avx512f_test (void)
{
  res1 = _mm_cvttsh_u32 (x1);
  res1 = _mm_cvtt_roundsh_u32 (x1, 8);
}
