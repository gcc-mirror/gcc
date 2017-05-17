/* PR target/80286 */
/* { dg-do run } */
/* { dg-options "-O2 -Wno-psabi" } */

typedef int V __attribute__((vector_size (4 * sizeof (int))));

__attribute__((noinline, noclone)) V
foo (V x, V y)
{
  return x << y[0];
}

int
main ()
{
  V x = { 1, 2, 3, 4 };
  V y = { 5, 6, 7, 8 };
  V z = foo (x, y);
  V e = { 1 << 5, 2 << 5, 3 << 5, 4 << 5 };
  if (__builtin_memcmp (&z, &e, sizeof (V)))
    __builtin_abort ();
  return 0;
}
