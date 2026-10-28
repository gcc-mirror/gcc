/* { dg-do compile } */
/* { dg-additional-options "-fopenmp-simd" } */

#pragma omp declare simd uniform(x) simdlen(4) notinbranch
extern void f(_Complex double x);

void g(int *restrict d, _Complex double x, int n)
{
#pragma omp simd
  for (int i = 0; i < n; i++)
    {
      f(x);
      d[i] = 1;
    }
}
