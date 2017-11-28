/* { dg-do compile } */
/* { dg-additional-options "-ftree-pre" } */

int pj;

void
g4 (unsigned long int *bc, unsigned long int *h5)
{
  if (pj != 0)
    {
      int ib = 0;

      while (bc != 0)
	{
m6:
	  for (pj = 0; pj < 2; ++pj)
	    pj = 0;

	  while (pj != 0)
	    {
	      for (;;)
		{
		}

	      while (ib != 0)
		{
		  unsigned long int tv = *bc;
		  unsigned long int n7;

		  *bc = 1;
		  while (*bc != 0)
		    {
		    }

ut:
		  if (pj == 0)
		    n7 = *h5 > 0;
		  else
		    {
		      *h5 = tv;
		      n7 = *h5;
		    }
		  ib += n7;
		}
	    }
	}

      goto ut;
    }

  goto m6;
}
