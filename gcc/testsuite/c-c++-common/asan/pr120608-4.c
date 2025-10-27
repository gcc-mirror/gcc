/* PR middle-end/120608 */
/* { dg-do compile { target musttail } } */
/* { dg-options "-fsanitize=address -fno-exceptions" } */

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
      if (a + b + c + x == 22)
	[[gnu::musttail]] return baz (x - 1);
      bar (&a, &b, &c);
    }
    bar (&a, &b, &a);
  }
  bar (&a, &a, &a);
  return 42;
}
