f (unsigned short Z[48])
{
  int j;
  unsigned short t1, t2, t3, T[48];
  unsigned short *p = T + 48;

  for (j = 1; j < 8; j++)
    {
      t1 = *Z++;
      *--p = *Z++;
      *--p = t1;
      t1 = inv(*Z++);
      t2 = -*Z++;
      t3 = -*Z++;
      *--p = inv(*Z++);
      *--p = t2;
      *--p = t3;
      *--p = t1;
    }
}
