/* PR tree-optimization/94589 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
foo (int x)
{
  return (x & 23) == x;
/* { dg-final { scan-tree-dump " & -24;" "optimized" } } */
/* { dg-final { scan-tree-dump-not " & 23;" "optimized" } } */
/* { dg-final { scan-tree-dump " == 0" "optimized" } } */
}

int
bar (int x)
{
  return (x | 137) != 137;
/* { dg-final { scan-tree-dump " & -138;" "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\| 137;" "optimized" } } */
/* { dg-final { scan-tree-dump " != 0" "optimized" } } */
}
