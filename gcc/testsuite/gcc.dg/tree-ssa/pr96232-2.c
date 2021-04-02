/* PR tree-optimization/96232 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump " 38 - " "optimized" } } */
/* { dg-final { scan-tree-dump " \\+ 97;" "optimized" } } */
/* { dg-final { scan-tree-dump-not "PHI <" "optimized" } } */

int
foo (_Bool x)
{
  return x ? 37 : 38;
}

int
bar (_Bool x)
{
  return x ? 98 : 97;
}
