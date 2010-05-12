/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

#include <emmintrin.h>

__m128d
foo (__m128d x, __m128d y)
{
  return _mm_cmpeq_sd (x, y);
}


/* { dg-final { scan-assembler "vcmpeqsd" } } */
