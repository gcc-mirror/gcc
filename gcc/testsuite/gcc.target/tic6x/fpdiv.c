/* { dg-do compile } */
/* { dg-require-effective-target ti_c67x } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "rcpdp" } } */
/* { dg-final { scan-assembler "rcpsp" } } */

double f (double x, double y)
{
  return x / y;
}

float g (float x, float y)
{
  return x / y;
}
