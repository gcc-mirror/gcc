/* PR middle-end/120608 */
/* { dg-do compile { target musttail } } */
/* { dg-options "-fsanitize=address -fno-exceptions" } */

[[gnu::noipa]] int
foo (int x)
{
  return x;
}

[[gnu::noipa]] void
bar (int *x, int *y, int *z)
{
  (void) x;
  (void) y;
  (void) z;
}

[[gnu::noipa]] int
baz (int x)
{
  int a = 4;
  {
    int b = 8;
    {
      int c = 10;
      bar (&a, &b, &c);
      if (a + b + c == 22)
	[[gnu::musttail]] return foo (x);
      bar (&a, &b, &c);
    }
    bar (&a, &b, &a);
  }
  bar (&a, &a, &a);
  return 42;
}
