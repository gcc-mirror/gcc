/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vmovw\[^-]" 1 } } */
/* { dg-final { scan-assembler-times "vpextrw" 1 } } */
#include <immintrin.h>

volatile __m128i x1;
volatile short x2;

void extern
avx512f_test (void)
{
  x1 = _mm_cvtsi16_si128 (x2);
  x2 = _mm_cvtsi128_si16 (x1);
}
