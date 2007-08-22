/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -mfpmath=387" } */

double a, b;
double f(double c)
{
  double x = a * b;
  return x + c * a;
}

/* { dg-final { scan-assembler-not "fld\[ \t\]*%st" } } */
/* { dg-final { scan-assembler "fmul\[ \t\]*%st" } } */
