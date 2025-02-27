/* PR tree-optimization/119030 */

static inline unsigned
foo (long long x)
{
  return x & 0x8000;
}

static inline long long
bar (long long x)
{
  if (foo (x))
    return -1000L;
  else
    return x >> 16;
}

long long x = -0x20000LL;

int
main ()
{
  if (bar (x) >= 0)
    __builtin_abort ();
  return 0;
}
