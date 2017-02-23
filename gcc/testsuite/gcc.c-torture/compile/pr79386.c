/* PR rtl-optimization/79386 */

int a, b;

int
foo (int x)
{
  int c;
  int *d, *e;

  if (b == 0)
    {
      c = 0;
      e = &b;
      d = &b;
    }
  else
    {
      int f;

      c = 1;
      for (f = 0; f < 9; ++f)
	c *= 3;
      e = (int *) (__UINTPTR_TYPE__) c;
      d = &x;
    }
  *e = c < 3;
  if (*e != 0)
    {
      int g;

      b += (a != 0) ? a : 1;
      if (g != 0 || x != 0)
	*d = 0;
      if (b >= 0)
	{
	  if (g != 0)
	    g = x;
	  if (*d / g != 0)
	    for (;;)
	      ;
	}
    }

  return b * (a != 0 && *d != 0);
}
