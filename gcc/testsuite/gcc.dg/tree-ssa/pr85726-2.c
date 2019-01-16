/* PR tree-optimization/85726 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump " / 3145728;" "optimized" } } */
/* { dg-final { scan-tree-dump "y = 0;" "optimized" } } */

int x, y;

void
foo (int n)
{
  int c = 3 << 20;
  x = n / c;
  y = x / c;
}
