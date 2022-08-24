/* PR rtl-optimization/104459 */
/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -fno-tree-dce -fcompare-debug -w" } */

void
foo (int x, int y)
{
  unsigned int a;

  for (;;)
    {
      short int *p = (short int *) &x;
      unsigned int q = 0;

      a /= 2;
      if (a)
	{
	  q -= y;
	  while (q)
	    ;
	}

      if (x)
	{
	  for (q = 0; q != 1; q += 2)
	    {
	      unsigned int n;

	      n = *p ? 0 : q;
	      y += n < 1;

	      n = a || *p;
	      if (n % x == 0)
		y /= x;
	    }
	}
    }
}
