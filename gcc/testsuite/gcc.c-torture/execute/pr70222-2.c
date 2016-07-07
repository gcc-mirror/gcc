/* PR rtl-optimization/70222 */

#if __CHAR_BIT__ == 8 && __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ == 8
__attribute__((noinline, noclone)) unsigned int
foo (int x)
{
  unsigned long long y = -1ULL >> x;
  return (unsigned int) y >> 31;
}
#endif

int
main ()
{
#if __CHAR_BIT__ == 8 && __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ == 8
  if (foo (15) != 1 || foo (32) != 1 || foo (33) != 0)
    __builtin_abort ();
#endif
  return 0;
}
