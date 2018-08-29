/* { dg-options "-O -ftree-vectorize" } */

int j4;

void
k1 (int ak)
{
  while (ak < 1)
    {
      int ur;

      for (ur = 0; ur < 2; ++ur)
        {
          ++j4;
          if (j4 != 0)
            j4 = 0;
        }

      ++ak;
    }
}
