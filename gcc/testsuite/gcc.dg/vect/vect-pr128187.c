/* { dg-do compile } */
/* { dg-additional-options "-ffast-math --param vect-partial-vector-usage=2" } */
/* { dg-additional-options "-march=x86-64-v4" { target { x86_64-*-* i?86-*-* } } } */

double f(double *a, int *d, int n, double s)
{
  for (int i = 0; i < n; i++)
    {
      s = __builtin_fma(a[i], a[i], s);
      d[i] = 1;
    }
  return s;
}
