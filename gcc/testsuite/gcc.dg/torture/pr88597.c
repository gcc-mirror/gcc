/* { dg-do compile } */
/* { dg-additional-options "-fpeel-loops --param max-completely-peel-times=30" } */

int
un (int dd)
{
  int nz, q8;

  for (nz = 0; nz < 3; ++nz)
    {
      int s2;

      q8 = dd;
      for (s2 = 0; s2 < 28; ++s2)
	q8 *= q8;
    }

  return q8;
}
