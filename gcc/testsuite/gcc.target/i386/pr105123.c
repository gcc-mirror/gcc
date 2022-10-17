/* PR target/105123 */
/* { dg-do run { target sse2_runtime } } */
/* { dg-options "-msse2" } */
/* { dg-additional-options "-mtune=i686" { target ia32 } } */

typedef unsigned short __attribute__((__vector_size__ (4 * sizeof (unsigned short)))) V;

V
foo (unsigned short u, V v)
{
  return __builtin_shuffle (u * v, v);
}

int
main ()
{
  V x = foo (1, (V) { 0, 1, 2, 3 });
  for (unsigned i = 0; i < 4; i++)
    if (x[i] != i)
      __builtin_abort ();
  return 0;
}
