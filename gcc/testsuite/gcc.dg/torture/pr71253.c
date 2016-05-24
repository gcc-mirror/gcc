/* { dg-do compile } */
/* { dg-additional-options "-ftree-loop-distribution" } */

int jo, af, yb;
long int wt;

void
nr (void)
{
  int *bf = &yb;
  for (;;)
    {
      while (jo != 0)
	{
	  long int *ad = (long int *) &yb;
	  for (;;)
	    {
	      int fv;
	      for (*ad = 1; *ad < 3; ++(*ad))
		{
		  af = *bf;
		  fv = wt;
		}
	      bf = (int *) &wt;
	      ad = &wt;
	      do
		{
		  jo = wt = ((wt != 0) ? 1 : fv);
		}
	      while (jo != 0);
	    }
	}
      bf = &af;
    }
}
