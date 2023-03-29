/* { dg-do compile } */
/* { dg-options "-fopenmp -O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return 42;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "return -1;" "optimized" } } */

int
foo (int x)
{
  int y;
  #pragma omp assume holds (x == 42)
  y = x;
  return y;
}

int
bar (int x)
{
  #pragma omp assume holds (x < 42)
  ;
  if (x == 42)
    return -1;
  return 42;
}
