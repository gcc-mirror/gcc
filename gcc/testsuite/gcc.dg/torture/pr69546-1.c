/* PR tree-optimization/69546 */
/* { dg-do run { target int128 } } */

unsigned __int128 __attribute__ ((noinline, noclone))
foo (unsigned long long x)
{
  unsigned __int128 y = ~0ULL;
  x >>= 63;
  return y / (x | 1);
}

unsigned __int128 __attribute__ ((noinline, noclone))
bar (unsigned long long x)
{
  unsigned __int128 y = ~33ULL;
  x >>= 63;
  return y / (x | 1);
}

int
main ()
{
  if (foo (1) != ~0ULL || bar (17) != ~33ULL)
    __builtin_abort ();
  return 0;
}
