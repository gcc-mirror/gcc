/* PR target/127897 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -mno-avx512f" } */
/* { dg-final { scan-assembler-times "vbroadcastss" 2 } } */

#include <immintrin.h>

__m128
f (__m128 x)
{
  return _mm_add_ps (_mm_mul_ps (x, _mm_set1_ps (1.5f)), _mm_set1_ps (0.25f));
}

