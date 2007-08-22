/* PR tree-optimization/24964 */
/* { dg-do compile } */
/* { dg-options "-O2 -mfpmath=387" } */

double fabs(double x);

double test1(double x)
{
  double t = fabs(x);
  return t*t;
}

double test2(double x)
{
  double t = -x;
  return t*t;
}

/* { dg-final { scan-assembler-not "fchs" } } */
/* { dg-final { scan-assembler-not "fabs" } } */
