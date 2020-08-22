/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

long int ar;
int dt;

long int
pc (unsigned long int q3, int zw)
{
  long int em = 0;

  while (zw < 1)
    {
      q3 = zw * 2ul;
      if (q3 != 0)
        for (ar = 0; ar < 2; ++ar)
          em = dt;

      ++zw;
    }

  return em;
}
