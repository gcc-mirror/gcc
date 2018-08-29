/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize" } */

int wx, qi;

void
yj (int gw)
{
  int *ak = &gw;

  while (wx != 0)
    {
      int k2 = (__INTPTR_TYPE__)&ak;
      int **xq = (int **)&k2;

ja:
      *xq = &gw;

      while (qi < 1)
	{
	  unsigned short int ey;

be:
	  for (ey = 0; ey < 251; ++ey)
	    {
	      for (wx = 0; wx < 2; ++wx)
		{
		}

	      *ak += 8555712;
	      k2 += *ak;
	    }
	  ++qi;
	}
    }

  gw = 1;
  if (gw != 0)
    goto ja;
  else
    goto be;
}
