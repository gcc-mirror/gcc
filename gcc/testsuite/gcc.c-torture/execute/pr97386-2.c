/* PR rtl-optimization/97386 */

__attribute__((noipa)) unsigned
foo (int x)
{
  unsigned long long a = (0x800000000000ccccULL << x) | (0x800000000000ccccULL >> (64 - x));
  unsigned int b = a;
  return (b << 24) | (b >> 8);
}

int
main ()
{
  if (__CHAR_BIT__ == 8
      && __SIZEOF_INT__ == 4
      &&  __SIZEOF_LONG_LONG__ == 8
      && foo (1) != 0x99000199U)
    __builtin_abort ();
  return 0;
}
