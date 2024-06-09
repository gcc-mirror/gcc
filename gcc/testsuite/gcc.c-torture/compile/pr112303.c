/* PR tree-optimization/112303 */

int a, b, d, e, f, **g, h;
char c;

int *
foo (void)
{
  for (int i = 0; i < 3; i++)
    {
      for (h = 0; h < 2; h++)
	;
      if (!b)
	break;
    }
  while (f)
    while (e)
      {
	c = 0;
	while (d)
	  while (a)
	    *g = foo ();
      }
  return 0;
}
