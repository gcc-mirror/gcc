/* PR c/120837 */
/* { dg-do run } */
/* { dg-options "-O1 -fsanitize=undefined -fno-sanitize-recover=undefined" } */

[[gnu::noipa]] void
bar (void **x, void **y)
{
  x[0] = 0;
  x[1] = 0;
  x[2] = 0;
  y[0] = 0;
  y[1] = 0;
  y[2] = 0;
  y[3] = 0;
  y[4] = 0;
}

[[gnu::noipa]] void *
foo (int x, int y)
{
  void *a[3];
  void *b[5];
  bar (a, b);
  return (x > y ? b : a)[y - 1];
}

int
main ()
{
  if (foo (2, 1) != 0)
    __builtin_abort ();
}
