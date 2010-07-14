/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -msse -mfpmath=sse -mrecip" } */
/* { dg-require-effective-target sse } */

float t1(float a, float b)
{
  return a / b;
}

/* { dg-final { scan-assembler "rcpss" } } */
