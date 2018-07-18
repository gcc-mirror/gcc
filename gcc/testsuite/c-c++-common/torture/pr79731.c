/* { dg-do compile } */
/* { dg-additional-options "-Wno-psabi -w" } */

typedef unsigned V __attribute__ ((vector_size (8)));
V
foo (unsigned x, V v)
{
  do {
      v %= x;
      x = 1;
  } while (v[1]);
  return v;
}
void fn2 ()
{
  V x = foo (5, (V) { 0, 1 });
  if (x[0] || x[1] || x[2] || x[3]);
}
