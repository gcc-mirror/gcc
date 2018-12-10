/* PR tree-optimization/85726 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " / 4;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " / 16;" 1 "optimized" } } */

int x, y, z;

void
foo (int n)
{
  x = n / 4;
  y = x / 4;
  z = y * 16 | 15;
}
