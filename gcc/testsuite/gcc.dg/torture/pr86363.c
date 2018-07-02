/* { dg-do run } */
/* { dg-additional-options "-w -Wno-psabi" } */

typedef char U __attribute__ ((vector_size (16)));
typedef unsigned V __attribute__ ((vector_size (16)));

V g;

V
f (V v, U u)
{
  __builtin_memset (&u[v[0]], 0, 1);
  g ^= u[0];
  return g;
}

int
main (void)
{
  V x = f ((V) { 5 }, (U) { 1 });

  if (x[0] != 1 || x[1] != 1 || x[2] != 1 || x[3] != 1)
    __builtin_abort ();
  return 0;
}
