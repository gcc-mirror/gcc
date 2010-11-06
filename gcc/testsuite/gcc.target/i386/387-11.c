/* { dg-do compile } */
/* { dg-options "-O2 -mfpmath=387 -mfancy-math-387" } */

double foo(double x, double y)
{
  double t = -x * y;
  return -t;
}

/* { dg-final { scan-assembler-not "fchs" } } */
