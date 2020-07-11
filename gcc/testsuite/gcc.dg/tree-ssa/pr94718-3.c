/* PR tree-optimization/94718 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-icf -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " \\\(int\\\) " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\\(unsigned int\\\) " 2 "optimized" } } */

int
f1 (int x, int y)
{
  return (int) ((unsigned) x | (unsigned) y);
}

int
f2 (int x, int y)
{
  unsigned a = x;
  unsigned b = y;
  return a | b;
}

int
f3 (int x, unsigned y)
{
  return (int) ((unsigned) x | y);
}

int
f4 (int x, unsigned y)
{
  unsigned a = x;
  return a | y;
}

unsigned
f5 (int x, unsigned y)
{
  return (unsigned) (x | (int) y);
}

unsigned
f6 (int x, unsigned y)
{
  int a = y;
  return x | a;
}
