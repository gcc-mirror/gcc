/* PR tree-optimization/94718 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-icf -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "= \[xy]_\[0-9]+\\\(D\\\) \\^ \[xy]_\[0-9]+\\\(D\\\);" 6 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\[0-9]+ < 0;" 6 "optimized" } } */

#define I (-__INT_MAX__ - 1)

int
f1 (int x, int y)
{
  return (x & I) != (y & I);
}

int
f2 (int x, int y)
{
  return (~x & I) != (~y & I);
}

int
f3 (int x, int y)
{
  return ((x & I) ^ I) != ((y & I) ^ I);
}

int
f4 (int x, int y)
{
  int s = (x & I);
  int t = (y & I);
  return s != t;
}

int
f5 (int x, int y)
{
  int s = (~x & I);
  int t = (~y & I);
  return s != t;
}

int
f6 (int x, int y)
{
  int s = ((x & I) ^ I);
  int t = ((y & I) ^ I);
  return s != t;
}
