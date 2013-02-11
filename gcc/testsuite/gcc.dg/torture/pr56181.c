/* { dg-do compile } */
/* { dg-options "-ftracer" } */

int a, b;

void f(void)
{
  if(a++)
    {
      for(a = 0; a < 1;)
	{
	  for(b = 0; b < 1; b++)
	    {
	      while(a++ < 0);
lbl:
	      ;
	    }

	  if(a)
	    goto lbl;
	}

      goto lbl;
    }
}
