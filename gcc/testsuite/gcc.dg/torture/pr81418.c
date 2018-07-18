/* { dg-do compile } */
/* { dg-additional-options "-ftree-loop-optimize" } */

int
ol (int ku)
{
  int zq = 0;

  while (ku < 1)
    {
      int y6;

      for (y6 = 0; y6 < 3; ++y6)
	zq += (char)ku;
      ++ku;
    }

  return zq;
}
