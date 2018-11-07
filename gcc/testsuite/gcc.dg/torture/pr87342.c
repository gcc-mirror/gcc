/* { dg-do compile } */

int ix;

void
o6 (int rh)
{
  if (rh == 0)
    {
      ix = 0;
      while (ix < 1)
	{
	}

      for (;;)
	if (ix == 0)
	  while (rh < 1)
	    {
	      if (rh == 0)
		{
		  __builtin_unreachable ();

kp:
		  if (ix == 0)
		    {
hk:
		      ix = 0;
		    }
		}

	      while (rh < 1)
		if (ix == 0)
		  goto kp;

	      while (rh < 1)
		{
		}
	    }
	else
	  goto kp;
    }

  goto hk;
}
