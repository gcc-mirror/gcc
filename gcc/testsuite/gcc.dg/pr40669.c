/* PR middle-end/40669 */
/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

double _Complex
test (int d, int t, double *x, double *y, double *z, int n,
      double _Complex (*fnp) (double))
{
  int m = n / 2;
  double min = y[t], max = z[t], med = x[m * d + t];
  double _Complex result = 0.0;

  if (n == 0)
    return 0.0;

  if (min > med)
    result += test (d, (t + 1) % d, x + (m + 1) * d, y, z, n - m - 1, fnp);
  else if (max < med)
    result += test (d, (t + 1) % d, x, y, z, m, fnp);
  else
    {
      result += fnp (y[0] + x[m]);
      result += test (d, (t + 1) % d, x + (m + 1) * d, y, z, n - m - 1, fnp);
    }
  return result;
}

