/* { dg-do compile } */

unsigned int lh;

void
ny (int t3, int ys, int rt, int p8)
{
  if (lh != 0)
    {
      if (0)
	{
oo:
	  do
	    {
	      rt = (p8 != 0) ? t3 : 0;
	      rt = (rt != 0 || lh != (unsigned int)ys);
	      rt += lh + ys;
	    }
	  while (t3 <= 0);

	  lh = ys;
	  ys = rt;
	}

      if (lh != 0)
	p8 = lh;
    }

  goto oo;
}
