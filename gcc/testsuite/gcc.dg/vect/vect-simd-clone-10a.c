/* { dg-do compile } */

#include "vect-simd-clone-10.h"

#pragma omp declare simd notinbranch
int
foo (long int a, int b, int c)
{
  return a + b + c;
}

#pragma omp declare simd notinbranch
long int
bar (int a, int b, long int c)
{
  return a + b + c;
}

