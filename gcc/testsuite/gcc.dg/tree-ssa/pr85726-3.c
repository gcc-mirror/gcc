/* PR tree-optimization/85726 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " / 3;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " / 15;" 1 "optimized" } } */

int x, y, z;

void
foo (int n)
{
  x = n / 3;
  y = x / 5;
  z = n / 15;
}
