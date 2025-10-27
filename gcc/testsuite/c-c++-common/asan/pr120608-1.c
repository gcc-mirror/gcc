/* PR middle-end/120608 */
/* { dg-do run { target musttail } } */
/* { dg-options "-O2 -fsanitize=address" } */

__attribute__((noipa)) void
foo (int *x, int *y, int *z)
{
  ++x[0];
  ++y[0];
  ++z[0];
}

__attribute__((noipa)) void
bar (int *x, int *y, int *z)
{
  if (x || y || z)
    __builtin_abort ();
}

__attribute__((noipa)) void
baz (int *x, int *y, int *z)
{
  (void) x; (void) y; (void) z;
  int a = 42, b = -42, c = 0;
  foo (&a, &b, &c);
  [[gnu::musttail]] return bar (0, 0, 0);
}

__attribute__((noipa)) void
qux (int *x, int *y, int *z)
{
  (void) x; (void) y; (void) z;
  int a = 42, b = -42, c = 0;
  foo (&a, &b, &c);
  [[gnu::musttail]] return bar (0, 0, 0);
}

int
main ()
{
  baz (0, 0, 0);
  qux (0, 0, 0);
}
