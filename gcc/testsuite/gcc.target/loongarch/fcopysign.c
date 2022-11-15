/* { dg-do compile } */
/* { dg-options "-mdouble-float" } */
/* { dg-final { scan-assembler "fcopysign\\.s" } } */
/* { dg-final { scan-assembler "fcopysign\\.d" } } */

double
my_copysign (double a, double b)
{
  return __builtin_copysign (a, b);
}

float
my_copysignf (float a, float b)
{
  return __builtin_copysignf (a, b);
}
