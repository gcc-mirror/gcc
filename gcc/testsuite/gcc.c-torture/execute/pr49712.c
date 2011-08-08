/* PR tree-optimization/49712 */

int a[2], b, c, d, e;

void
foo (int x, int y)
{
}

int
bar (void)
{
  int i;
  for (; d <= 0; d = 1)
    for (i = 0; i < 4; i++)
      for (e = 0; e; e = 1)
	;
  return 0;
}

int
main ()
{
  for (b = 0; b < 2; b++)
    while (c)
      foo (a[b] = 0, bar ());
  return 0;
}
