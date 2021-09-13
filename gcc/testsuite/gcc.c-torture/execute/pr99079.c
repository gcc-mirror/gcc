/* PR tree-optimization/99079 */

__attribute__((noipa)) unsigned long long
foo (int x)
{
  unsigned long long s = 1 << x;
  return 4897637220ULL % s;
}

int
main ()
{
  if (__SIZEOF_INT__ * __CHAR_BIT__ != 32)
    return 0;
  if (foo (31) != 4897637220ULL)
    __builtin_abort ();
  return 0;
}
