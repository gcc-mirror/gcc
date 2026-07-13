/* { dg-do compile } */
/* { dg-options "-O1 -ftrapping-math -fdump-tree-optimized -fdump-tree-phiopt-details" } */
/* PR tree-optimization/126138 */

/* (eq || trap) -> trap is fine as eq will be false for NaN
   which means it is not short circuit and will cause a trap
   on the trapping instruction always.  */
int
f (double i, double j, int n, int m)
{
  // (i == j) || (i < j)
  if (i == j)
    return 1;
  return i < j;
}
/* (ne && trap) -> trap has a story. */
int
f1 (double i, double j, int n, int m)
{
  // (i != j) || (i < j)
  if (i != j)
    return (i < j);
  return 0;
}
/* { dg-final { scan-tree-dump-not " if " "optimized" } } */
/* { dg-final { scan-tree-dump-times " < " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " <= " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "changed to factor operation out from COND_EXPR" 2 "phiopt1" } } */
