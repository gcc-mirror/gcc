/* { dg-do run } */
/* { dg-additional-options "-w -Wno-psabi" } */

typedef unsigned V __attribute__ ((vector_size (16)));

V
foo (unsigned x, V v)
{
  do {
      v %= x;
      x = 1;
  } while (v[1]);
  return v;
}

int
main ()
{
  V x = foo (5, (V) { 0, 1 });
  if (x[0] || x[1] || x[2] || x[3])
    __builtin_abort();
  return 0;
}
