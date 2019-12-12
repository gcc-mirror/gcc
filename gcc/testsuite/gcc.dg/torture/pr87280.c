/* { dg-do compile } */
/* { dg-additional-options "--param rpo-vn-max-loop-depth=5" } */

int uc;

void
j8 (int *xv, int f3)
{
  uc = 0;
  while (uc < 1)
    {
    }

  if (*xv == 0)
    {
      int *o8 = xv;

      if (0)
	{
n3:
	  *o8 = 0;
	  while (*o8 < 1)
	    {
h5:
	      *o8 = 0;
	    }
	}

      while (*xv < 1)
	if (*xv == 0)
	  goto h5;

g5:
      ;
    }

  *xv = 0;
  for (;;)
    {
      while (uc < 1)
	{
	}

      while (f3 < 1)
	{
	  if (*xv == 0)
	    goto n3;

	  while (f3 < 1)
	    while (*xv < 1)
	      while (*xv < 1)
		while (*xv < 1)
		  while (*xv < 1)
		    {
		    }
	}

      if (*xv == 0)
	goto g5;
    }
}
