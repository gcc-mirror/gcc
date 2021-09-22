/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vcvttsh2si\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%rax" 1 } } */
/* { dg-final { scan-assembler-times "vcvttsh2si\[ \\t\]+\{sae\}\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%rax" 1 } } */

#include <immintrin.h>

volatile __m128h x1;
volatile long long res2;

void extern
avx512f_test (void)
{
  res2 = _mm_cvttsh_i64 (x1);
  res2 = _mm_cvtt_roundsh_i64 (x1, 8);
}
