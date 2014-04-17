/* { dg-do run } */

int a, d, e, f, g, h, i, j, k;
unsigned short b;

short
fn1 (int p1, int p2)
{
  return p1 * p2;
}

int
main ()
{
  for (; a; a--)
    {
      int l = 0;
      if (f >= 0)
	{
	  for (; h;)
	    e = 0;
	  for (; l != -6; l--)
	    {
	      j = fn1 (b--, d);
	      for (g = 0; g; g = 1)
		;
	      k = e ? 2 : 0;
	    }
	  i = 0;
	  for (;;)
	    ;
	}
    }
  d = 0;
  return 0;
}
