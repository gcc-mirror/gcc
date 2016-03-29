/* PR rtl-optimization/70429 */

__attribute__((noinline, noclone)) int
foo (int a)
{
  return (int) (0x14ff6e2207db5d1fLL >> a) >> 4;
}

int
main ()
{
  if (sizeof (int) != 4 || sizeof (long long) != 8 || __CHAR_BIT__ != 8)
    return 0;
  if (foo (1) != 0x3edae8 || foo (2) != -132158092)
    __builtin_abort ();
  return 0;
}
