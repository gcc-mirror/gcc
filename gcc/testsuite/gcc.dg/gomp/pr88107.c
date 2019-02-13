/* PR tree-optimization/88107 */
/* { dg-do compile { target fgraphite } } */
/* { dg-require-effective-target vect_simd_clones } */
/* { dg-options "-O2 -fexceptions -floop-nest-optimize -fnon-call-exceptions -fopenmp-simd -ftree-parallelize-loops=2" } */

#define N 1024
int a[N], b[N];
long int c[N];
unsigned char d[N];

#pragma omp declare simd notinbranch
__attribute__((noinline)) static int
foo (long int a, int b, int c)
{
  return a + b + c;
}

#pragma omp declare simd notinbranch
__attribute__((noinline)) static long int
bar (int a, int b, long int c)
{
  return a + b + c;
}

void
baz (void)
{
  int i;
  #pragma omp simd
  for (i = 0; i < N; i++)
    a[i] = foo (c[i], a[i], b[i]) + 6;
  #pragma omp simd
  for (i = 0; i < N; i++)
    c[i] = bar (a[i], b[i], c[i]) * 2;
}
