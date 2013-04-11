/* PR tree-optimization/56899 */

#if __SIZEOF_INT__ == 4 && __CHAR_BIT__ == 8
__attribute__((noinline, noclone)) void
f1 (int v)
{
  int x = -214748365 * (v - 1);
  if (x != -1932735285)
    __builtin_abort ();
}

__attribute__((noinline, noclone)) void
f2 (int v)
{
  int x = 214748365 * (v + 1);
  if (x != -1932735285)
    __builtin_abort ();
}

__attribute__((noinline, noclone)) void
f3 (unsigned int v)
{
  unsigned int x = -214748365U * (v - 1);
  if (x != -1932735285U)
    __builtin_abort ();
}

__attribute__((noinline, noclone)) void
f4 (unsigned int v)
{
  unsigned int x = 214748365U * (v + 1);
  if (x != -1932735285U)
    __builtin_abort ();
}
#endif

int
main ()
{
#if __SIZEOF_INT__ == 4 && __CHAR_BIT__ == 8
  f1 (10);
  f2 (-10);
  f3 (10);
  f4 (-10U);
#endif
  return 0;
}
