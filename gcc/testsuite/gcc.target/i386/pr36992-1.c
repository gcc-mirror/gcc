/* { dg-do compile }
/* { dg-options "-O2 -msse2" } */

#include <emmintrin.h>

__m128i
test (__m128i b)
{
  return _mm_move_epi64 (b);
}

/* { dg-final { scan-assembler-times "movq\[ \\t\]+\[^\n\]*%xmm" 1 } } */
