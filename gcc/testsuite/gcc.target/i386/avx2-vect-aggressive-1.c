/* { dg-do compile } */
/* { dg-options "-mavx2 -O3 -fopenmp-simd -fdump-tree-vect-details" } */

#define N 256
int a1[N], a2[N], a3[N], a4[N], a5[N], a6[N], a7[N];

void foo()
{
  int x1, x2, x3;
  int i;
#pragma omp simd safelen(8)
  for (i=0; i<N; i++)
    {
      x1 = a1[i] + a2 [i];
	if (x1 >= 0 && x1 != 3)
	  {
	    x2 = a3[i] - a1[i];
	    if (x2 >= 0 && x2 != 4)
	      {
		x3 = a4[i] + a5[i];
		if (x3 >= 0 && x3 != 5)
		  {
		    a6[i] = x1 - x2;
		    a7[i] = x3 + x2;
		  }
	      }
	  }
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
