void matmul_i1 ()
{
  int *abase;
  int aystride;
  int x, n, count, xcount;
  int *dest_y;
  int *abase_n;
  for (n = 0; n < count; n++)
    {
      abase_n = abase + n * aystride;
      for (x = 0; x < xcount; x++)
	dest_y[x] += abase_n[x];
    }
}

