/* { dg-do run } */

__attribute__((noipa))
int
fplus (int x, int m, int n)
{
  if (x > -6 || m < -3 || m > -2 || n < -2 || n > -1)
    return 0;
  return (x + m * n) / n;
}

__attribute__((noipa))
int
fminus (int x, int m, int n)
{
  if (x > -6 || m < 2 || m > 3 || n < -2 || n > -1)
    return 0;
  return (x - m * n) / n;
}

int
main (void)
{
  if (fplus (-2147483647 - 1, -2, -1) != 2147483646
      || fminus (-2147483647 - 1, 2, -1) != 2147483646)
    __builtin_abort ();
  return 0;
}
