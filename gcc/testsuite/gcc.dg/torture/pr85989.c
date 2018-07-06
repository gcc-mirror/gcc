/* { dg-do run } */

#define N 9

void __attribute__((noipa))
f (double x, double y, double *res)
{
  y = -y;
  for (int i = 0; i < N; ++i)
    {
      double tmp = y;
      y = x;
      x = tmp;
      res[i] = i;
    }
  res[N] = y * y;
  res[N + 1] = x;
}

int
main (void)
{
  double res[N + 2];
  f (10, 20, res);
  for (int i = 0; i < N; ++i)
    if (res[i] != i)
      __builtin_abort ();
  if (res[N] != 100 || res[N + 1] != -20)
    __builtin_abort ();
  return 0;
}
