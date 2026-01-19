/* PR tree-optimization/123656 */
/* { dg-do run } */
/* { dg-options "-Og -Wno-psabi" } */

#define C 210

typedef __attribute__((__vector_size__ (2))) unsigned char V;

V
foo (V v)
{
  return v * C;
}

int
main ()
{
  V x = foo ((V) {1, 1});
  if (x[0] != C || x[1] != C)
    __builtin_abort ();
}
