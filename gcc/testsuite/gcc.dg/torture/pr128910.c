/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

__attribute__((noipa)) int
f (unsigned long long x, int b)
{
  int t = 0, r = 0;
  unsigned lo = x;
  for (int i = 0; i < 3 && (unsigned) x; i++)
    {
      if (b || t)
	r = x == lo;
      t = x = i != 0;
    }
  return r;
}

int
main (void)
{
  if (f (0x100000001ULL, 1) != 0)
    __builtin_abort ();
  __builtin_exit (0);
}
