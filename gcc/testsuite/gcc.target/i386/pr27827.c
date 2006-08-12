/* { dg-do compile { target ilp32 } } */
/* { dg-options "-O2" } */

double a, b;
double f(double c)
{
  double x = a * b;
  return x + c * a;
}

/* { dg-final { scan-assembler-not "fld\[ \t\]*%st" } } */
/* { dg-final { scan-assembler "fmul\[ \t\]*%st" } } */
