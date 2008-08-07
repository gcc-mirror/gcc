/* { dg-do compile }
/* { dg-options "-O2 -msse4" } */

#include <emmintrin.h>

__m128i
test (__m128i b)
{
  return _mm_move_epi64 (b);
}

/* { dg-final { scan-assembler-times "movq\[ \\t\]+.*%xmm" 1 } } */
