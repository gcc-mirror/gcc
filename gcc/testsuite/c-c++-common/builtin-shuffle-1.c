/* PR c++/78089 */
/* { dg-do run } */

typedef int V __attribute__((vector_size (16)));
V a, b, c;

int
foo ()
{
  return __builtin_shuffle (a, b, c)[3];
}

int
main ()
{
  a = (V) { 1, 2, 3, 4 };
  b = (V) { 5, 6, 7, 8 };
  c = (V) { 7, 2, 5, 6 };
  if (foo () != 7)
    __builtin_abort ();
  return 0;
}
