/* PR target/98853 */

#if __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ == 8
__attribute__((noipa)) unsigned long long
foo (unsigned long long x, unsigned int y)
{
  return ((unsigned) x & 0xfffe0000U) | (y & 0x1ffff);
}
#endif

int
main ()
{
#if __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ == 8
  if (foo (0xdeadbeefcaf2babeULL, 0xdeaffeedU) != 0x00000000caf3feedULL)
    __builtin_abort ();
#endif
  return 0;
}
