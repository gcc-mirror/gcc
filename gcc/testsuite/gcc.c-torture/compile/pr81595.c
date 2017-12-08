/* PR rtl-optimization/81595 */

void
foo (__INTPTR_TYPE__ *x, int *y, int *z, int u, int v)
{
  while (u != 0)
    {
      if (*x != 0)
	{
	  int a = 1;
 l1:
	  if (*y != 0)
	    {
	      while (a < 2)
		{
		  a = 0;
		  x = (__INTPTR_TYPE__ *)&x;
 l2:
		  ++a;
		}
	      while (*z != 0)
		;
	    }
	  a /= 0;
	}
      else
	{
	  *z /= (*z != 0) ? 2 : 0;
	  while (v < 1)
	    {
	      *y = 0;
	      if (v != 0)
		goto l1;
	      ++v;
	    }
	  goto l2;
	}
    }
}
