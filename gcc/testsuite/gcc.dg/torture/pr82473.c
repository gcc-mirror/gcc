/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

void
zz (int x9, short int gt)
{
  if (0)
    {
      while (gt < 1)
	{
	  int pz;

k6:
	  for (pz = 0; pz < 3; ++pz)
	    x9 += gt;
	  ++gt;
	}
    }

  if (x9 != 0)
    goto k6;
}
