/* { dg-do compile } */
/* { dg-options "-O2 -mlsx" } */
/* { dg-final { scan-assembler "vand\\.v" } } */
/* { dg-final { scan-assembler "vxor\\.v" } } */
/* { dg-final { scan-assembler-not "fcopysign" } } */
/* { dg-final { scan-assembler-not "fmul" } } */

double
my_xorsign (double a, double b)
{
  return a * __builtin_copysign (1.0d, b);
}

float
my_xorsignf (float a, float b)
{
  return a * __builtin_copysignf (1.0f, b);
}
