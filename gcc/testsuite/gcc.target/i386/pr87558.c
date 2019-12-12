/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

#include <emmintrin.h>

void foo (unsigned char *p, __m128i x)
{
  _mm_storeu_si64 ((void *)p, x);
}
