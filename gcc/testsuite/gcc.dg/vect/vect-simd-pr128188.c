/* { dg-do compile } */
/* { dg-additional-options "-fopenmp-simd -fno-tree-loop-im -fno-tree-pre" } */

#pragma omp declare simd linear(p:1) notinbranch
extern int update(int *p, int x);

void f(int *restrict d, const int *restrict a, int n, int m)
{
#pragma omp simd
  for (int i = 0; i < n; i++)
    {
      int t = a[i];
#pragma omp simd
      for (int j = 0; j < m; j++)
	d[i*m+j] = update(&t, a[j]);
    }
}
