/* PR tree-optimization/95731 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " >= 0\| < 0" 6 "optimized" } } */

int
foo (int x, int y, int z, int w, long long u, long long v)
{
  return x >= 0 && y >= 0 && z < 0 && u < 0 && w >= 0 && v < 0;
}

int
bar (int x, int y, int z, int w, long long u, long long v)
{
  return u >= 0 && x >= 0 && y >= 0 && v < 0 && z >= 0 && w >= 0;
}

int
baz (int x, int y, int z, int w, long long u, long long v)
{
  return x >= 0 || u < 0 || y >= 0 || v < 0 || z >= 0 || w >= 0;
}
