/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mfpmath=sse" } */

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

/* { dg-final { scan-assembler-times "vblendvp\[sd]" 2 } } */
