/* PR target/105613 */
/* { dg-do run { target int128 } } */

typedef unsigned __int128 __attribute__((__vector_size__ (16))) V;

void
foo (V v, V *r)
{
  *r = v != 0;
}

int
main ()
{
  V r;
  foo ((V) {5}, &r);
  if (r[0] != ~(unsigned __int128) 0)
    __builtin_abort ();
  foo ((V) {0x500000005ULL}, &r);
  if (r[0] != ~(unsigned __int128) 0)
    __builtin_abort ();
  foo ((V) {0}, &r);
  if (r[0] != 0)
    __builtin_abort ();
  return 0;
}
