/* { dg-do compile } */
/* { dg-options "-O2 -msse3 -mfpmath=sse -mtune-ctrl=v2df_reduction_prefer_haddpd" } */

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

__m128d h (__m128d p, __m128d q)
{
  __m128d r = { p[0] - p[1], q[0] - q[1] };
  return r;
}

__m128d i1 (__m128d p, __m128d q)
{
  __m128d r = { p[0] + p[1], q[0] + q[1] };
  return r;
}

__m128d i2 (__m128d p, __m128d q)
{
  __m128d r = { p[0] + p[1], q[1] + q[0] };
  return r;
}

__m128d i3 (__m128d p, __m128d q)
{
  __m128d r = { p[1] + p[0], q[0] + q[1] };
  return r;
}

__m128d i4 (__m128d p, __m128d q)
{
  __m128d r = { p[1] + p[0], q[1] + q[0] };
  return r;
}

/* { dg-final { scan-assembler-times "hsubpd" 2 } } */
/* { dg-final { scan-assembler-times "haddpd" 6 } } */
/* { dg-final { scan-assembler-not "unpck" } } */
