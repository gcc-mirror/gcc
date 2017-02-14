/* PR target/64252 */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target int32plus } */

typedef unsigned int V __attribute__((vector_size (32)));

__attribute__((noinline, noclone)) void
foo (V *a, V *b, V *c, V *d, V *e)
{
  V t = __builtin_shuffle (*a, *b, *c);
  V v = __builtin_shuffle (t, (V) { ~0U, ~0U, ~0U, ~0U, ~0U, ~0U, ~0U, ~0U }, (V) { 0, 1, 8, 3, 4, 5, 9, 7 });
  v = v + *d;
  *e = v;
}

int
main ()
{
  V a, b, c, d, e;
  int i;
  a = (V) { 1, 2, 3, 4, 5, 6, 7, 8 };
  b = (V) { 9, 10, 11, 12, 13, 14, 15, 16 };
  c = (V) { 1, 3, 5, 7, 9, 11, 13, 15 };
  d = (V) { 0, 0, 0, 0, 0, 0, 0, 0 };
  foo (&a, &b, &c, &d, &e);
  for (i = 0; i < 8; i++)
    if (e[i] != ((i == 2 || i == 6) ? ~0U : 2 + 2 * i))
      __builtin_abort ();
  return 0;
}
