/* PR rtl-optimization/78378 */

unsigned long long __attribute__ ((noinline, noclone))
foo (unsigned long long x)
{
  x <<= 41;
  x /= 232;
  return 1 + (unsigned short) x;
}

int
main ()
{
  unsigned long long x = foo (1);
  if (x != 0x2c24)
    __builtin_abort();
  return 0;
}
