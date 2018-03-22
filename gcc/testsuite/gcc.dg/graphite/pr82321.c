/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize" } */

int y8;

void
dm (int io)
{
  if (y8 != 0)
    {
      int pu = 1;

      while (io < 2)
	{
	  int xo = (pu != 0) ? y8 : 0;

	  while (y8 != 0)
	    if (xo != 0)
	      {
gi:
		xo = (__INTPTR_TYPE__)&io;
		pu = 0;
	      }
	}
    }

  if (io != 0)
    {
      y8 = 1;
      while (y8 != 0)
	if (io / !y8 != 0)
	  y8 = 0;

      goto gi;
    }
}
