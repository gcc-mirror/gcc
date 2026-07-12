/* { dg-do compile } */
/* { dg-options "-O1 -ftrapping-math -fdump-tree-optimized -fdump-tree-ifcombine-details" } */
/* PR tree-optimization/126138 */

/* (eq || trap) -> trap is fine as eq will be false for NaN
   which means it is not short circuit and will cause a trap
   on the trapping instruction always.  */
int
f (double i, double j, int n, int m)
{
  // (i <= j) || (i < j) -> i <= j
  if (i <= j)
    return m;
  if (i < j)
    return m;
  return n;
}
/* (ne && trap) -> trap has a story. */
int
f1 (double i, double j, int n, int m)
{
  // (i <= j) && (i < j) -> i <= j
  if (i <= j)
    if (i < j)
      return m;
  return n;
}

/* { dg-final { scan-tree-dump-times "optimizing trapping cond to" 2 "ifcombine" } } */
/* { dg-final { scan-tree-dump-times " if " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " < " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " <= " 1 "optimized" } } */

