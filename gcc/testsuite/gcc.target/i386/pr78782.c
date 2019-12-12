/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

#include <emmintrin.h>

__m128i foo (unsigned char *p)
{
  return _mm_loadu_si64 ((void *)p);
}
