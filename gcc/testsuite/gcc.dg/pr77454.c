/* PR tree-optimization/77454 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (unsigned char x, char y)
{
  while (x != 0)
    {
      unsigned char *a = &x;
      int b;

      if (y != 0)
	a = (unsigned char *) &y;
      else if (y + 1 != 0)
	a = (unsigned char *) &y;
      for (x = 0; x < 1; ++x)
	b = 0;
      for (y = 0; y < 3; ++y)
	{
	  y = !!y;
	  if (y != 0)
	    x = y;
	}
      if ((b != 0 ? -1 : *a) < (y = b))
	b = 1;
    }
}
