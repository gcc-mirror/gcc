/* { dg-do compile } */
/* { dg-additional-options "-fvect-cost-model=dynamic" } */

int m;

void
foo (int p[][16], unsigned int x)
{
  while (x < 4)
    {
      int j;

      for (j = x * 4; j < (x + 1) * 4 - 2; j++)
        p[0][j] = p[m][j];

      ++x;
    }
}
