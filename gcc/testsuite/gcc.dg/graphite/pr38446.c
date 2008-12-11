/* { dg-options "-O2 -fgraphite-identity" } */

void copy_data()
{
  int ****source;
  int ****dest;

  int i, j, k, l;
  for (i = 0; i < 10; i++)
      for (k = 0; k < 2; k++)
        for (l = 0; l < 65; l++)
          source[i][j][k][l] = dest[i][j][k][l];
}

