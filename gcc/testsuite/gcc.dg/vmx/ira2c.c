/* { dg-do compile } */

double __fabs(double a) { return a; }
double __fmadd(double a, double b, double c) { return a*b+c; }

double
test(double f32a, double f32b, double f32c)
{
  f32c = __fabs(f32a);
  return __fmadd(f32a, f32b, f32c);
}
