/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-dce -fno-tree-dse -fopenmp-simd" } */
/* { dg-additional-options "-mavx2" { target { x86_64-*-* i?86-*-* } } } */

#pragma omp declare simd simdlen(4) inbranch
extern int f(int) __attribute__((const));

void g(int *restrict d, int n)
{
#pragma omp simd
  for (int i = 0; i < n; i++)
    if (d[i] > 0)
      {
        int u = f(d[i]);
        d[i]++;
      }
}
