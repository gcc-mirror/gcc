/* PR tree-optimization/52073 */

int a, b, c, d, e, f;

void
foo (int x)
{
  e = 1;
  for (;;)
    {
      int g = c;
      if (x)
	{
	  if (e)
	    continue;
	  while (a)
	    --f;
	}
      else
	for (b = 5; b; b--)
	  {
	    d = g;
	    g = 0 == d;
	  }
      if (!g)
	x = 0;
    }
}
