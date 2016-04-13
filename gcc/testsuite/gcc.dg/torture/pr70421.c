/* PR target/70421 */
/* { dg-do run } */
/* { dg-additional-options "-Wno-psabi -w" } */

typedef unsigned V __attribute__ ((vector_size (64)));

unsigned __attribute__ ((noinline, noclone))
foo (unsigned x, V u, V v)
{
  v[1] ^= v[2];
  x ^= ((V) v)[u[0]];
  return x;
}

int
main ()
{
  unsigned x = foo (0x10, (V) { 1 }, (V) { 0x100, 0x1000, 0x10000 });
  if (x != 0x11010)
    __builtin_abort ();
  return 0;
}
