/* { dg-options "-O1 -funsafe-math-optimizations -floop-interchange" } */

double f(double x)
{
  double y = 0.0;
  int i;
  for (i = 0; i < 8; i++) {
    y += x * i;
  }
  return y;
}
