/* PR tree-optimization/83269 */

int
main ()
{
#if __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ > 4 && __CHAR_BIT__ == 8
  volatile unsigned char a = 1;
  long long b = 0x80000000L;
  int c = -((int)(-b) - (-0x7fffffff * a));
  if (c != 1)
    __builtin_abort ();
#endif
  return 0;
}
