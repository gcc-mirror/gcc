/* PR tree-optimization/109393 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo(int *a, int j)
{
  int k = j - 1;
  return a[j - 1] == a[k];
}

int foo2(int *a, int j)
{
  int k = j - 5;
  return a[j - 5] == a[k];
}

int bar(int *a, int j)
{
  int k = j - 1;
  return (&a[j + 1] - 2) == &a[k];
}

/* { dg-final { scan-tree-dump-times "return 1;" 3 "optimized" } } */
