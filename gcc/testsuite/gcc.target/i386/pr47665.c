/* PR target/47665 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

#include <emmintrin.h>

__m128d
foo (double *x, __m128i y)
{
  return _mm_load_pd (x + _mm_cvtsi128_si32 (_mm_srli_si128 (_mm_slli_epi32 (y, 2), 0)));
}
