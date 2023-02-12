/* PR tree-optimization/95771 */
/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -mpopcnt -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump " = __builtin_popcount| = \\.POPCOUNT" "optimized" } } */

int
corge (unsigned __int128 x)
{
  int i = 0;
  while (x)
    {
      x &= x - 1;
      ++i;
    }
  return i;
}
