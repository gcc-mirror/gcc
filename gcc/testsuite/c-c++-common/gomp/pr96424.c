/* PR tree-optimization/96424 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -O0 -fexceptions -fnon-call-exceptions -fprofile-use -Wno-missing-profile" } */

void
foo (void)
{
  int i, j;
#pragma omp for collapse (2)
  for (i = 0; i < 10; ++i)
    for (j = 0; j <= i; ++j)
      ;
}

void
bar (void)
{
  int i, j;
#pragma omp for collapse (2)
  for (i = 0; i < 10; ++i)
    for (j = 0; j < i; ++j)
      ;
}
