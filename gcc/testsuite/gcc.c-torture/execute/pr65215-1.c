/* PR tree-optimization/65215 */

static inline unsigned int
foo (unsigned int x)
{
  return (x >> 24) | ((x >> 8) & 0xff00) | ((x << 8) & 0xff0000) | (x << 24);
}

__attribute__((noinline, noclone)) unsigned int
bar (unsigned long long *x)
{
  return foo (*x);
}

int
main ()
{
  if (__CHAR_BIT__ != 8 || sizeof (unsigned int) != 4 || sizeof (unsigned long long) != 8)
    return 0;
  unsigned long long l = foo (0xdeadbeefU) | 0xfeedbea800000000ULL;
  if (bar (&l) != 0xdeadbeefU)
    __builtin_abort ();
  return 0;
}
