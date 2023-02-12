/* PR tree-optimization/108064 */

static inline short
foo (short value)
{
  return ((value >> 8) & 0xff) | ((value & 0xff) << 8);
}

__attribute__((noipa))
void
bar (short *d, const short *s)
{
  for (unsigned long i = 0; i < 4; i++)
    d[i] = foo (s[i]);
}

int
main ()
{
  short a[4] __attribute__((aligned (16))) = { 0xff, 0, 0, 0 };
  short b[4] __attribute__((aligned (16)));
  short c[4] __attribute__((aligned (16)));

  bar (b, a);
  bar (c, b);
  if (a[0] != c[0])
    __builtin_abort ();
}
