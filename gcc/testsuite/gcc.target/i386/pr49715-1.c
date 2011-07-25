/* { dg-do compile } */
/* { dg-options "-O2 -msse -mfpmath=sse" } */

float func(unsigned x)
{
  return (x & 0xfffff) * 0.01f;
}

/* { dg-final { scan-assembler-times "cvtsi2ss" 1 } } */
