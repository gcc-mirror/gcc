/* { dg-do compile } */
/* { dg-options "-O3 -mavx512f -fdump-tree-vect-details" } */

void foo (int n, int *off, double *a)
{
  const int m = 32;

  for (int j = 0; j < n/m; ++j)
    {
      int const start = j*m;
      int const end = (j+1)*m;

#pragma GCC ivdep
      for (int i = start; i < end; ++i)
	{
	  a[off[i]] = a[i] < 0 ? a[i] : 0;
	}
    }
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
