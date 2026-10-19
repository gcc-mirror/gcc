/* { dg-do run } */

/* The loops exit on a header PHI whose latch value is the modified IV, but
   whose start value is not the start value of the IV.  */

__attribute__((noipa)) int
clz_complement (unsigned x, unsigned y)
{
  unsigned t = x, u = y;
  int b = 0;
  for (;;)
    {
      t >>= 1;
      b++;
      if (u == 0)
	break;
      u = t;
    }
  return b;
}

__attribute__((noipa)) int
popcount (unsigned x, unsigned y)
{
  unsigned t = x, u = y;
  int b = 0;
  for (;;)
    {
      t &= t - 1;
      b++;
      if (u == 0)
	break;
      u = t;
    }
  return b;
}

__attribute__((noipa)) int
ctz (unsigned x, unsigned y)
{
  if (x == 0)
    return -1;
  unsigned t = x, u = y;
  int b = 0;
  for (;;)
    {
      t >>= 1;
      b++;
      if (u & 1)
	break;
      u = t;
    }
  return b;
}

int
main ()
{
  if (clz_complement (0, 1) != 2 || clz_complement (8, 0) != 1)
    __builtin_abort ();
  if (popcount (0, 1) != 2 || popcount (7, 0) != 1)
    __builtin_abort ();
  if (ctz (4, 1) != 1 || ctz (4, 2) != 3)
    __builtin_abort ();
  return 0;
}
