/* PR tree-optimization/96691 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " \\\| 123;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\\& 123;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\\^ -315;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\\^ 314;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\\^ 321;" "optimized" } } */
/* { dg-final { scan-tree-dump-not " = ~" "optimized" } } */

int
foo (int x)
{
  return (~x | 123) ^ 321;
}

int
bar (int x)
{
  return (~x & 123) ^ 321;
}
