/* { dg-do compile } */
/* { dg-options "-msse4.1 -O2 -mno-sse4.2" } */
/* { dg-final { scan-assembler-times {(?n)blendvpd[ \t]+%xmm[0-9]+} 1 } } */

#include <immintrin.h>

__m128d
foo (__m128d a, __m128d b, __m128d c)
{
  return _mm_blendv_pd (a, b, c);
}
