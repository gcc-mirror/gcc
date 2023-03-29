/* PR tree-optimization/95771 */
/* { dg-do compile } */
/* { dg-options "-O2 -mpopcnt -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " = __builtin_popcount| = \\.POPCOUNT" 4 "optimized" } } */

int
foo (unsigned char x)
{
  int i = 0;
  while (x)
    {
      x &= x - 1;
      ++i;
    }
  return i;
}

int
bar (unsigned short x)
{
  int i = 0;
  while (x)
    {
      x &= x - 1;
      ++i;
    }
  return i;
}

int
baz (unsigned int x)
{
  int i = 0;
  while (x)
    {
      x &= x - 1;
      ++i;
    }
  return i;
}

int
qux (unsigned long long x)
{
  int i = 0;
  while (x)
    {
      x &= x - 1;
      ++i;
    }
  return i;
}
