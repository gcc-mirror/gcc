/* PR target/109040 */

typedef unsigned short __attribute__((__vector_size__ (32))) V;

unsigned short a, b, c, d;

void
foo (V m, unsigned short *ret)
{
  V v = 6 > ((V) { 2124, 8 } & m);
  unsigned short uc = v[0] + a + b + c + d;
  *ret = uc;
}

int
main ()
{
  unsigned short x;
  foo ((V) { 0, 15 }, &x);
  if (x != (unsigned short) ~0)
    __builtin_abort ();
  return 0;
}
