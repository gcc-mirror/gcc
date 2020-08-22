/* { dg-do compile } */

double foo (double x, int n)
{
  double s = 0.;
  for (int i = 0; i < n; ++i)
    {
      s += x;
      s += x;
      s += x;
    }
  return s;
}
