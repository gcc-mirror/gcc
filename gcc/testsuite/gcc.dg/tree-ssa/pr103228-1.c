/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
int f(int a, int b)
{
  b|=1u;
  b|=2;
  return b;
}
/* { dg-final { scan-tree-dump-times "\\\| 3" 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times "\\\| 1" 0 "optimized"} } */
/* { dg-final { scan-tree-dump-times "\\\| 2" 0 "optimized"} } */
