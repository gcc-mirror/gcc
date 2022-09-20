/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-msse4.1 -O2 -Ofast" } */
/* { dg-final { scan-assembler-times "roundps" 9 } } */
/* { dg-final { scan-assembler-times "cvtps2dq" 1 } } */
/* { dg-final { scan-assembler-times "cvttps2dq" 3 } } */

#include<math.h>

void
foo (float* p, float* __restrict q)
{
  p[0] = truncf (q[0]);
  p[1] = truncf (q[1]);
}

void
foo1 (float* p, float* __restrict q)
{
  p[0] = floorf (q[0]);
  p[1] = floorf (q[1]);
}

void
foo1i (int* p, float* __restrict q)
{
  p[0] = (int) floorf (q[0]);
  p[1] = (int) floorf (q[1]);
}

void
foo2 (float* p, float* __restrict q)
{
  p[0] = ceilf (q[0]);
  p[1] = ceilf (q[1]);
}

void
foo2i (int* p, float* __restrict q)
{
  p[0] = (int) ceilf (q[0]);
  p[1] = (int) ceilf (q[1]);
}

void
foo3 (float* p, float* __restrict q)
{
  p[0] = rintf (q[0]);
  p[1] = rintf (q[1]);
}

void
foo3i (int* p, float* __restrict q)
{
  p[0] = (int) rintf (q[0]);
  p[1] = (int) rintf (q[1]);
}

void
foo4 (float* p, float* __restrict q)
{
  p[0] = nearbyintf (q[0]);
  p[1] = nearbyintf (q[1]);
}

void
foo5(float* p, float* __restrict q)
{
  p[0] = roundf (q[0]);
  p[1] = roundf (q[1]);
}

void
foo5i(int* p, float* __restrict q)
{
  p[0] = (int) roundf (q[0]);
  p[1] = (int) roundf (q[1]);
}
