void
foo (int y, int z, unsigned char **t, int **c, int *b)
{
  int i, j, k;
  unsigned char a[2];

  a[0] = 0;
  a[1] = 0;
  for (j = 0; j < z; j++)
    for (i = 0; i < y; i++, a[0] += 3)
      for (k = 0; k < 3; k++)
	c[0][k] += 3 * b[k];
  for (i = 0; i < 3; i++)
    if (t[0][i] + c[0][i] / a[0] <= 0)
      t[0][i] = 0;
    else
      t[0][i] = t[0][i] + c[0][i] / a[0];
}
