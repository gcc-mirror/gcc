/* PR tree-optimization/86835 */
/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -Wuninitialized" } */

__attribute__((noipa)) void
foo (int n, double *x, double *y)
{		/* { dg-bogus "is used uninitialized in this function" "" { target *-*-* } 0 } */
  int i;
  double b = y[4];
  for (i = 0; i < n; ++i)
    y[3] += __builtin_sin (x[i] / b);
  y[0] /= b;
  y[1] /= b * b;
  y[2] /= b;
}

int
main ()
{
  double y[] = { 16.0, 64.0, 128.0, 0.0, 2.0 };
  foo (0, y, y);
  if (__builtin_fabs (y[0] - 8.0) > 0.0001
      || __builtin_fabs (y[1] - 16.0) > 0.0001
      || __builtin_fabs (y[2] - 64.0) > 0.0001
      || y[3] != 0.0
      || y[4] != 2.0)
    __builtin_abort ();
  return 0;
}
