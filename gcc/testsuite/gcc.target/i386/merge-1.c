/* { dg-do compile } */
/* { dg-options "-O1 -msse2 -mno-sse4" } */

#include <x86intrin.h>

void
f (double *r, __m128d x, __m128d y, __m128d z)
{
  __m128d t=_mm_move_sd(x,y);
  __m128d u=_mm_move_sd(t,z);
  *r = u[0];
}

__m128d
g(__m128d x, __m128d y, __m128d z)
{
  __m128d t=_mm_move_sd(x,y);
  __m128d u=_mm_move_sd(t,z);
  return u;
}

/* { dg-final { scan-assembler-times "movsd" 1 } } */
