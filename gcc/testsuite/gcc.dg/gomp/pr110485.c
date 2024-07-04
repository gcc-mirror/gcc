/* PR 110485 */
/* { dg-do compile } */
/* { dg-additional-options "-Ofast -fdump-tree-vect-details" } */
/* { dg-additional-options "-march=znver4 --param=vect-partial-vector-usage=1" { target x86_64-*-* } } */
#pragma omp declare simd notinbranch uniform(p)
extern double __attribute__ ((const)) bar (double a, double p);

double a[1024];
double b[1024];

void foo (int n)
{
  #pragma omp simd
  for (int i = 0; i < n; ++i)
    a[i] = bar (b[i], 71.2);
}

/* { dg-final { scan-tree-dump-not "MASK_LOAD" "vect" } } */
/* { dg-final { scan-tree-dump "can't use a fully-masked loop because a non-masked simd clone was selected." "vect" { target x86_64-*-* } } } */
