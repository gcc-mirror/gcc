/* PR tree-optimization/65215 */

static inline unsigned int
foo (unsigned int x)
{
  return (x >> 24) | ((x >> 8) & 0xff00) | ((x << 8) & 0xff0000) | (x << 24);
}

__attribute__((noinline, noclone)) unsigned long long
bar (unsigned long long *x)
{
  return ((unsigned long long) foo (*x) << 32) | foo (*x >> 32);
}

int
main ()
{
  if (__CHAR_BIT__ != 8 || sizeof (unsigned int) != 4 || sizeof (unsigned long long) != 8)
    return 0;
  unsigned long long l = foo (0xfeedbea8U) | ((unsigned long long) foo (0xdeadbeefU) << 32);
  if (bar (&l) != 0xfeedbea8deadbeefULL)
    __builtin_abort ();
  return 0;
}
