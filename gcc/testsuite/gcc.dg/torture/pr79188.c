/* { dg-do run } */

int a, b, c, d, e, f, h, j, k;

static void fn1 ()
{
  int g = 1, i;
  if (!f)
    {
      for (; d < 1; d++)
	for (i = 0, j = 1; i < 1; i = j)
	  a = 2;
      for (; e < 1; e++)
	{
	  for (; k; k++)
	    L:
		;
	  for (c = 0; c < 2; c++)
	    {
	      for (i = 0; i < 4; i++)
		{
		  for (; h; h++)
		    g = 0;
		  b = 0;
		}
	      if (b)
		goto L;
	    }
	  a = 0;
	}
      if (g < 0)
	goto L;
    }
}

int main ()
{
  fn1 ();

  if (a != 0) 
    __builtin_abort ();

  return 0; 
}
