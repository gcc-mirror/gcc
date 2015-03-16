/* PR tree-optimization/65427 */

typedef int V __attribute__ ((vector_size (8 * sizeof (int))));
V a, b, c, d, e, f;

__attribute__((noinline, noclone)) void
foo (int x, int y)
{
  do
    {
      if (x)
	d = a ^ c;
      else
	d = a ^ b;
    }
  while (y);
}

int
main ()
{
  a = (V) { 1, 2, 3, 4, 5, 6, 7, 8 };
  b = (V) { 0x40, 0x80, 0x40, 0x80, 0x40, 0x80, 0x40, 0x80 };
  e = (V) { 0x41, 0x82, 0x43, 0x84, 0x45, 0x86, 0x47, 0x88 };
  foo (0, 0);
  if (__builtin_memcmp (&d, &e, sizeof (V)) != 0)
    __builtin_abort ();
  c = (V) { 0x80, 0x40, 0x80, 0x40, 0x80, 0x40, 0x80, 0x40 };
  f = (V) { 0x81, 0x42, 0x83, 0x44, 0x85, 0x46, 0x87, 0x48 };
  foo (1, 0);
  if (__builtin_memcmp (&d, &f, sizeof (V)) != 0)
    __builtin_abort ();
  return 0;
}
