/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

#include <xmmintrin.h>

__m128
foo (__m128 x, __m128 y)
{
  return _mm_cmpeq_ss (x, y);
}


/* { dg-final { scan-assembler "vcmpeqss" } } */
