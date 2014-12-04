/* PR tree-optimization/64006 */

int v;

long __attribute__ ((noinline, noclone))
test (long *x, int y)
{
  int i;
  long s = 1;
  for (i = 0; i < y; i++)
    if (__builtin_mul_overflow (s, x[i], &s))
      v++;
  return s;
}

int
main ()
{
  long d[7] = { 975, 975, 975, 975, 975, 975, 975 };
  long r = test (d, 7);
  if (sizeof (long) * __CHAR_BIT__ == 64 && v != 1)
    __builtin_abort ();
  else if (sizeof (long) * __CHAR_BIT__ == 32 && v != 4)
    __builtin_abort ();
  return 0;
}
