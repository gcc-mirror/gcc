/* { dg-do compile } */

void
wn (int ki)
{
  int m8 = 0;
  int *d6 = &ki;

  if (ki == 0)
    {
ud:
      for (ki = 0; ki < 1; ++ki)
	for (m8 = 0; m8 < 1; ++m8)
	  goto ud;

      d6 = &m8;

y8:
      ++m8;

xw:
      if (ki == 0)
	{
	}
      else
	{
	  for (m8 = 0; m8 < 1; ++m8)
	    {
gt:
	      if (*d6 == 0)
		goto y8;
	    }

	  for (m8 = 0; m8 < 1; ++m8)
	    {
	      goto gt;

ym:
	      ;
	    }
	}

      d6 = &ki;

      goto ym;
    }

  goto xw;
}
