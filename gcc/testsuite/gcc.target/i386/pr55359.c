/* { dg-do compile } */
/* { dg-options "-mavx2 -O2" } */

#include <x86intrin.h>

__m128d
f (__m256d x)
{
  return *((__m128d*) ((double *) &x + 1));
}
