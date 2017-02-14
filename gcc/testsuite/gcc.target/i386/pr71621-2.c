/* { dg-do compile } */
/* { dg-options "-O3 -mavx2" } */

int hf, sv, zz, aj;

void
dn (int xb, int bl)
{
  while (zz < 1)
    {
      if (xb == 0)
	goto mr;

      while (bl < 3)
	{
	  int d3;
	  unsigned char vh;
	  unsigned char *fj = &vh;

	mr:
	  while (bl < 1)
	    {
	      hf += vh;
	      ++bl;
	    }
	  if (xb == 0)
	    zz = bl;
	  if (d3 == 0)
	    return;
	  while (sv < 1)
	    {
	      --vh;
	      aj += vh;
	      ++sv;
	    }
	}
      sv = 0;
    }
}
