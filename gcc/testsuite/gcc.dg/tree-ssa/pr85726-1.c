/* PR tree-optimization/85726 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump " / 16;" "optimized" } } */
/* { dg-final { scan-tree-dump " / 3;" "optimized" } } */
/* { dg-final { scan-tree-dump " % 3;" "optimized" } } */
/* { dg-final { scan-tree-dump-not " / 48;" "optimized" } } */

int ww, vv;

int
foo (int y)
{
  int z = y / 16;
  int w = z / 3;
  int v = z % 3;
  ww = w;
  return v;
}
