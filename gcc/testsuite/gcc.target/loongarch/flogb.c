/* { dg-do compile } */
/* { dg-options "-mdouble-float -fno-math-errno" } */
/* { dg-final { scan-assembler "fabs\\.s" } } */
/* { dg-final { scan-assembler "fabs\\.d" } } */
/* { dg-final { scan-assembler "flogb\\.s" } } */
/* { dg-final { scan-assembler "flogb\\.d" } } */

double
my_logb (double a)
{
  return __builtin_logb (a);
}

float
my_logbf (float a)
{
  return __builtin_logbf (a);
}
