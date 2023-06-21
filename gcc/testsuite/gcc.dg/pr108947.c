/* PR target/108947 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-forward-propagate -Wno-psabi" } */

typedef unsigned short __attribute__((__vector_size__ (2 * sizeof (short)))) V;

__attribute__((__noipa__)) V
foo (V v)
{
  V w = 3 > (v & 3992);
  return w;
}

int
main ()
{
  V w = foo ((V) { 0, 9 });
  if (w[0] != 0xffff || w[1] != 0)
    __builtin_abort ();
  return 0;
}
