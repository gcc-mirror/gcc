/* PR tree-optimization/102705 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */
/* { dg-final { scan-tree-dump "a == 0" "original" } } */
/* { dg-final { scan-tree-dump "b == 0" "original" } } */

int
f1 (int a)
{
  return (1 >> a);
}

int
f2 (int b)
{
  return ((1 >> b) & 1);
}
