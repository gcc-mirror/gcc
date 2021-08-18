/* { dg-do compile } */
/* { dg-options "-O2 -msse3 -mfpmath=sse" } */

#include <x86intrin.h>

double f (__m128d p)
{
  return p[0] - p[1];
}

double g1 (__m128d p)
{
  return p[0] + p[1];
}

double g2 (__m128d p)
{
  return p[1] + p[0];
}

/* { dg-final { scan-assembler-not "hsubpd" } } */
/* { dg-final { scan-assembler-not "haddpd" } } */
