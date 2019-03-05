/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1 -mno-avx -mfpmath=sse" } */

__attribute__((noipa)) double
f1 (double a, double b)
{
  return a < 0 ? a : b;
}

__attribute__((noipa)) float
f2 (float a, float b)
{
  return a < 0 ? a : b;
}

/* { dg-final { scan-assembler-times "blendvp\[sd]" 2 } } */
