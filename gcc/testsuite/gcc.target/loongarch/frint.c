/* { dg-do compile } */
/* { dg-options "-mdouble-float" } */
/* { dg-final { scan-assembler "frint\\.s" } } */
/* { dg-final { scan-assembler "frint\\.d" } } */

double
my_rint (double a)
{
  return __builtin_rint (a);
}

float
my_rintf (float a)
{
  return __builtin_rintf (a);
}
