/* { dg-do compile } */
/* { dg-additional-options "-fstrict-overflow -ftree-vrp" } */

void
vr (int of, unsigned char bw)
{
  int d1;
  int lm = 0;

  for (d1 = 0; d1 < 3; ++d1)
    {
      const int vl = 2;

      while (bw < vl)
	{
	}
      if (bw != vl)
	lm -= vl;
    }
  while (++of < 1)
    {
      lm /= bw;
      of += lm;
    }
}
