/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-ccp -fno-tree-forwprop" } */

void
tp (void)
{
  int qt;

  qt = 0;
  if (qt != 0)
    {
      if (0)
	{
h5:
	  qt = 0;
	  while (qt < 1)
	    {
	    }
	}

      ++qt;
    }

  goto h5;
}
