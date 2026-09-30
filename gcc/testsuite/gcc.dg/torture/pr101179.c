/* { dg-do run } */

static int __attribute__((noinline))
f (int a, int b, int x, int p, int q)
{
  if (x > -1 || x < -100)
    return 5;
  int y;
  if (a)
    y = p;
  else
    y = q;
  int m = x % y;
  if (b)
    return 7;
  if (m == 0)
    return 1;
  return 2;
}

int
main (void)
{
  volatile int one = 1, zero = 0, v = -100;
  if (f (one, zero, v, 64, 16) != 2)
    __builtin_abort ();
  return 0;
}
