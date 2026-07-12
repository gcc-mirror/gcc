/* { dg-do compile } */
/* { dg-options "-O2 -ftrapping-math -fdump-tree-original -fdump-tree-optimized" } */
/* PR tree-optimization/126138 */

/* (eq || trap) -> trap is fine as eq will be false for NaN
   which means it is not short circuit and will cause a trap
   on the trapping instruction always.  */
int
f (double i, double j)
{
  return (i == j) || (i < j);
}
/* (ne && trap) -> trap has a story. */
int
f1 (double i, double j)
{
  return (i != j) && (i < j);
}
/* { dg-final { scan-tree-dump-not " && " "original" } } */
/* { dg-final { scan-tree-dump-not " \\\|\\\| " "original" } } */
/* { dg-final { scan-tree-dump-not " if " "optimized" } } */
