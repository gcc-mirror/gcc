/* PR target/93637 */
/* { dg-do compile } */
/* { dg-options "-mavx -mno-avx2 -O3 --param sccvn-max-alias-queries-per-access=3" } */

double
foo (void)
{
  int i;
  double r = 7.0;
  double a[] = { 0.0, 0.0, -0.0, 0.0, 0.0, -0.0, 1.0, 0.0, 0.0, -0.0, 1.0, 0.0, 1.0, 1.0 };

  for (i = 0; i < sizeof (a) / sizeof (a[0]); ++i)
    if (a[i] == 0.0)
      r = a[i];

  return r;
}
