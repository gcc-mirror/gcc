static inline int foo (long x)
{
  register int a = 0;
  register unsigned b;

  do
    {
      b = (x & 0x7f);
      x = (x >> 7) | ~(-1L >> 7);
      a += 1;
    }
  while ((x != 0 || (b & 0x40) != 0) && (x != -1 || (b & 0x40) == 0));
  return a;
}

static inline int bar (unsigned long x)
{
  register int a = 0;
  register unsigned b;

  do
    {
      b = (x & 0x7f);
      x >>= 7;
      a++;
    }
  while (x != 0);
  return a;
}

int
baz (unsigned long x, int y)
{
  if (y)
    return foo ((long) x);
  else
    return bar (x);
}
