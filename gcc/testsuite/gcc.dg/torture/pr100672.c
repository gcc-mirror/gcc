/* { dg-do run } */
/* { dg-additional-options "-w -Wno-psabi" } */

typedef long long __attribute__((__vector_size__ (4 * sizeof (long long)))) V;

V
foo (V v)
{
  return -(v >> 1);
}

int
main (void)
{
  V v = foo ((V) { -2, -4, -6, -8 });
  if (v[0] != 1 || v[1] != 2 || v[2] != 3 || v[3] != 4)
    __builtin_abort ();
  return 0;
}
