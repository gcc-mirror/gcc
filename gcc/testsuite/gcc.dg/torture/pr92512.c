/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

long int
nl (long int fy, int k3, int zr)
{
  while (k3 < 1)
    {
      if (zr == 0)
        fy = 0;

      fy *= fy < zr;
      ++k3;
    }

  return fy;
}
