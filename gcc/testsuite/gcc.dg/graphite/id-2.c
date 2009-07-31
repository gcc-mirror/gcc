typedef _Complex float GFC_COMPLEX_4;
matmul_c4 ()
{
  int x, n, count;
  GFC_COMPLEX_4 * bbase_y;
  GFC_COMPLEX_4 * dest_y;
  GFC_COMPLEX_4 * abase_n;
  GFC_COMPLEX_4 bbase_yn;

  for (n = 0; n < count; n++)
    {
      bbase_yn = bbase_y[n];
      for (x = 0; x < count; x++)
        dest_y[x] += abase_n[x] * bbase_yn;
    }
}
