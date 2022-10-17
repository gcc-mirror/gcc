/* { dg-do compile } */
/* { dg-options "-mavx512f -O2 -mfpmath=sse -ffast-math" } */
/* { dg-final { scan-assembler-times "vcvtsi2s\[sd\]" "2" } } */
/* { dg-final { scan-assembler-times "vscalefs\[sd\]" "2" } } */

double
__attribute__((noipa))
foo (double a, int b)
{
  return __builtin_ldexp (a, b);
}

float
__attribute__((noipa))
foo2 (float a, int b)
{
  return __builtin_ldexpf (a, b);
}
