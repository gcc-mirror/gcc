/* PR tree-optimization/94801 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return 0;" 2 "optimized" } } */

int
foo (int a)
{
  return __builtin_clz (a) >> 5;
}

int
bar (int a)
{
  return __builtin_ctz (a) >> 5;
}
