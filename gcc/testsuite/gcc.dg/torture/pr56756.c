/* { dg-do compile } */

int a, *b;

void f(void)
{
  if(a)
    {
      int k;

      for(a = 0; a < 1; a++)
	{
	  int **q;
	  f();

	  for(; **q; ++**q)
	    lbl:
		if(a)
		  {
		    a = 0;
		    goto lbl;
		  }

	  b = &k;
	}
    }
  goto lbl;
}
