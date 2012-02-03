/* PR rtl-optimization/52092 */

int a, b, c, d, e, f, g;

void
foo (void)
{
  for (;;)
    {
      int *h = 0;
      int i = 3;
      int **j = &h;
      if (e)
	{
	  c = d || a;
	  g = c ? a : b;
	  if ((char) (i * g))
	    {
	      h = &f;
	      *h = 0;
	    }
	  **j = 0;
	}
    }
}
