/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -ffast-math -msse -mfpmath=sse -mrecip" } */

float t1(float a, float b)
{
  return a / b;
}

/* { dg-final { scan-assembler "rcpss" } } */
