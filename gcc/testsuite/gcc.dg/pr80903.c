/* PR rtl-optimization/80903 */
/* { dg-do compile } */
/* { dg-options "-O3 -funroll-loops" } */

short int a;

void
foo (int x, short int y, short int z)
{
  if (y != 0)
    {
      const short int b = 37;
      y = 0;
      while (y < b)
	{
	  while (y < b)
	    {
	    lab:
	      ++y;
	    }
	  for (y = 0; y < b - 1; ++y)
	    ;
	  if (z != 0)
	    {
	      --a;
	      y *= a;
	    }
	  z = x;
	}
      x = 0;
    }

  goto lab;
}
