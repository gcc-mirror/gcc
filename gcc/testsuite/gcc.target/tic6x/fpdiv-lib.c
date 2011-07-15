/* { dg-do compile } */
/* { dg-require-effective-target ti_c67x } */
/* { dg-options "-O2 -fno-reciprocal-math" } */
/* { dg-final { scan-assembler-not "rcpdp" } } */
/* { dg-final { scan-assembler-not "rcpsp" } } */

double f (double x, double y)
{
  return x / y;
}

float g (float x, float y)
{
  return x / y;
}
