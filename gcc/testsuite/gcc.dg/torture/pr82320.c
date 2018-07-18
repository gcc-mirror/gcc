/* { dg-do compile } */

void
ec (int n4, short int ea)
{
  if (1)
    {
      if (ea != 0)
	{
	  int *c1 = (int *)&ea;

nn:
	  for (;;)
	    ++*c1;
	}
    }
  else
    {
      int *lq = &n4;
      int *md;
      int da;

      goto nn;

r1:
      md = lq;
      for (da = 0; da < 1; ++da)
	{
ig:
	  ++n4;
	  *md += n4;
	}
    }

  for (ea = 0; ea < 1; ++ea)
    goto r1;

  goto ig;
}
