/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-ch -fno-tree-vrp" } */

int x0;

void
br (int yp, int oo)
{
  int *qi = &yp;

  if (oo == 0)
    {
g8:
      if (x0 != 0)
	x0 = yp;
      else if (oo != 0)
	x0 = yp;

      if (x0 == 0)
	{
	  *qi = 0;
	  x0 = *qi;
	}

      if (x0 != 0)
	{
	  ++oo;
	  goto g8;
	}

      if (yp == oo)
	yp += !!oo;
    }
  else
    {
      x0 = 1;
      while (x0 < 2)
	{
	  qi = &oo;
	  ++oo;
	  x0 = 1;
	}
    }

  goto g8;
}
