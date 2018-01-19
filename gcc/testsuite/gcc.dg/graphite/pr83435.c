/* { dg-do compile { target pthread } } */
/* { dg-options "-O -ftree-parallelize-loops=2 -floop-parallelize-all" } */

int yj, ax;

void
gf (signed char mp)
{
  int *dh = &yj;

  for (;;)
    {
      signed char sb;

      for (sb = 0; sb < 1; sb -= 8)
	{
	}

      mp &= mp <= sb;
      if (mp == 0)
	dh = &ax;
      mp = 0;
      *dh = 0;
    }
}
