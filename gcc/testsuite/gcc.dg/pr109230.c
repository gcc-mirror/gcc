/* PR tree-optimization/109230 */
/* { dg-do run } */
/* { dg-options "-O2 -Wno-psabi" } */

#if __SIZEOF_FLOAT__ == __SIZEOF_INT__
typedef float V __attribute__((vector_size (4 * sizeof (float))));
typedef int VI __attribute__((vector_size (4 * sizeof (float))));

__attribute__((noipa)) V
foo (V x, V y)
{
  V a = x - y;
  V b = y + x;
  return __builtin_shuffle (b, a, (VI) { 0, 5, 2, 3 });
}

int
main ()
{
  V a = (V) { 1.0f, 2.0f, 3.0f, 4.0f };
  V b = (V) { 8.0f, 9.0f, 10.0f, 11.0f };
  V c = foo (a, b);
  if (c[0] != 9.0f || c[1] != -7.0f || c[2] != 13.0f || c[3] != 15.0f)
    __builtin_abort ();
}
#else
int
main ()
{
}
#endif
