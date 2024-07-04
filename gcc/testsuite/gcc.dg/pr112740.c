/* { dg-do run { target { int128 } } } */
/* { dg-options "" } */

typedef unsigned __int128 __attribute__((__vector_size__ (16))) V;

V
foo (unsigned c, V v)
{
  return (V) (c <= v) == 0;
}

int
main (void)
{
  V x = foo (0, (V) { });
  if (x[0])
    __builtin_abort ();
  return 0;
}
