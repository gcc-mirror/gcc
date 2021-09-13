/* PR tree-optimization/56719 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " > 1023" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " > 2047" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " > 8191" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " <= 1023" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " <= 4095" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " <= 8191" 1 "optimized" } } */

int
f1 (int x, int y)
{
  return x > 0x3ffU || y > 0x3ffU;
}

int
f2 (int x, int y, int z, unsigned w)
{
  return x > 0x1fffU || z > 0x7ffU || w > 0x7ffU || y > 0x1fffU;
}

int
f3 (int x, int y)
{
  return x <= 0x3ffU && y <= 0x3ffU;
}

int
f4 (int x, int y, unsigned z, unsigned w)
{
  return x <= 0x1fffU && z <= 0xfff && w <= 0xfff && y <= 0x1fffU;
}
