/* PR tree-optimization/53163 */

struct S { int s; } b, f;
int a, c;

void
foo (void)
{
  int d, e;
  for (d = 4; d < 19; ++d)
    for (e = 2; e >= 0; e--)
      {
	a = 0;
	a = 1;
      }
}

void
bar (void)
{
  int g, h, i;
  for (i = 1; i >= 0; i--)
    {
      b = f;
      for (g = 0; g <= 1; g++)
	{
	  if (c)
	    break;
	  for (h = 0; h <= 1; h++)
	    foo ();
	  foo ();
	}
    }
}
