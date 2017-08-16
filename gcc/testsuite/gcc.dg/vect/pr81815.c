/* { dg-do run } */

int __attribute__ ((noinline, noclone))
f (int *x, int n)
{
  int b = 13;
  for (int i = 0; i < n; ++i)
    {
      int next = x[i];
      b = b < 100 ? next : 200;
    }
  return b;
}

static int res[32];

int
main (void)
{
  for (int i = 0; i < 32; ++i)
    res[i] = i;
  res[15] = 100;
  if (f (res, 32) != 200)
    __builtin_abort ();
  return 0;
}
