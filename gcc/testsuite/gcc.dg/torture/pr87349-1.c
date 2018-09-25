/* { dg-do compile } */

void
h1 (int *fh, int pw)
{
  *fh = 0;
  if (*fh != 0)
    for (;;)
      {
	fh = &pw;

	if (pw == 0)
	  {
	  }
	else
	  while (pw < 1)
	    {
	      if (pw == 0)
		{
ut:
		  ;
		}

	      ++pw;
	    }

	if (pw == 0)
	  goto ut;
      }

  goto ut;
}

