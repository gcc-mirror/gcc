/* { dg-do compile } */
/* { dg-options "-O2 -msse -mfpmath=sse -ffinite-math-only" } */

float foo (float a, float b)
{
  return a < b ? a : b;
}

float bar (float a, float b)
{
  return a > b ? a : b;
}

/* { dg-final { scan-assembler-times "minss" 1 } } */
/* { dg-final { scan-assembler-times "maxss" 1 } } */
