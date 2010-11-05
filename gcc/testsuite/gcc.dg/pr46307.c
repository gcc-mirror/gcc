/* PR debug/46307 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

double fma (double, double, double);

double
foo (double x, double y, double z)
{
  double a = x * y + z;
  double b = __builtin_fma (x, y, z);
  double c = fma (x, y, z);
  return x / y / z;
}
