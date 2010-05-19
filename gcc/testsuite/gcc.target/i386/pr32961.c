/* { dg-do compile } */
/* { dg-options "-O0 -msse2" } */
/* { dg-require-effective-target sse2 } */

#include <xmmintrin.h>

void x (int n)
{
  __m128i a;
  a = _mm_slli_epi32 (a, n);
}
