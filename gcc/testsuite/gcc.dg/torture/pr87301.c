/* { dg-do compile } */
/* { dg-additional-options "-Wno-div-by-zero" } */

void
bl (int *be)
{
  int lo;
    {
      int **ny;

      if (*be == 0)
	{
	  int ***k8 = &ny;
	  int uj = (__INTPTR_TYPE__)&lo;

	  for (;;)
	    if (***k8 == 0)
	      {
		uj = !!(1 / 0) ? !(lo = 0) : 0;
		(void) uj;

		if (*ny == 0)
		  for (;;)
		    if (***k8 == 0)
		      {
		      }

		for (lo = 0; lo < 2; ++lo)
		  {
		  }
	      }
	}
    }
}
