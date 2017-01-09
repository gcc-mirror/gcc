/* PR tree-optimization/78899 */
/* { dg-do compile } */
/* { dg-options "-Ofast -fopenmp-simd -mavx2 -mno-avx512f" } */

#define N 1024
#define M 4
int p1[N], p2[N], p3[N], c[N];

void
foo (int n)
{
  int i, k;
  for (k = 0; k < n / M; k++)
    {
    #pragma omp simd
      for (i = 0; i < M; i++)
	if (c[k * M + i])
	  {
	    p1[k * M + i] += 1;
	    p2[k * M + i] = p3[k * M + i] + 2;
	  }
    }
}

/* Ensure the loop is vectorized.  */
/* { dg-final { scan-assembler "vpmaskmov" } } */
/* { dg-final { scan-assembler "vpadd" } } */
