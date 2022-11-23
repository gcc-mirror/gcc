/* { dg-do compile } */
/* { dg-options "-mavx512bf16 -O2" } */
/* { dg-additional-options "-fno-PIE -mfpmath=sse" { target ia32 } } */
/* { dg-final { scan-assembler-times "pslld" 1 } } */

#include <immintrin.h>

volatile __bfloat16 x1;
volatile float res;

void extern
avx512bf16_test (void)
{
  res = _mm_cvtsbh_ss (x1);
}
