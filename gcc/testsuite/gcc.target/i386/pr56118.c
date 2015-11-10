/* { dg-do compile } */
/* { dg-options "-O3 -msse2" } */

#include <emmintrin.h>

__m128d f()
{
  __m128d r={3,4};
  r[0]=1;
  r[1]=2;
  return r;
}

/* We want to "vectorize" this to a aligned vector load from the
   constant pool.  */

/* { dg-final { scan-assembler "movapd" } } */
