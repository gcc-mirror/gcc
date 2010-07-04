/* This testcase caused a buffer overflow in simplify_immed_subreg.  */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

#include <emmintrin.h>

__m128i foo (__m128i x)
{
  return _mm_min_epu8 (x, _mm_set1_epi8 (10));
}
