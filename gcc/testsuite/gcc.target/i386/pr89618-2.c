/* { dg-do compile } */
/* { dg-options "-O3 -mavx2 -mno-avx512f -fdump-tree-vect-details" } */

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

/* Make sure the cost model selects SSE vectors rather than AVX to avoid
   too many scalar ops for the address computes in the loop. 
  
   Since open-coded scatters are costed wrong, we no longer vectorize after fixing
   COND_EXPR costs.  See PR119902.  */
/* { dg-final { scan-tree-dump "loop vectorized using 16 byte vectors" "vect" { target { ! ia32 } xfail *-*-*  } } } */
/* { dg-final { scan-tree-dump-not "loop vectorized using 32 byte vectors" "vect" { target { ! ia32 } } } } */
