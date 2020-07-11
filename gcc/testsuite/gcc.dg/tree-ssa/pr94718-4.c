/* PR tree-optimization/94718 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-icf -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "= \[xy]_\[0-9]+\\\(D\\\) \\^ \[xy]_\[0-9]+\\\(D\\\);" 8 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\[0-9]+ < 0;" 8 "optimized" } } */

int
f1 (int x, int y)
{
  return (x < 0) != (y < 0);
}

int
f2 (int x, int y)
{
  return (x >= 0) != (y >= 0);
}

int
f3 (int x, int y)
{
  return (x < 0) == (y >= 0);
}

int
f4 (int x, int y)
{
  return (x >= 0) == (y < 0);
}

int
f5 (int x, int y)
{
  int s = (x < 0);
  int t = (y < 0);
  return s != t;
}

int
f6 (int x, int y)
{
  int s = (x >= 0);
  int t = (y >= 0);
  return s != t;
}

int
f7 (int x, int y)
{
  int s = (x < 0);
  int t = (y >= 0);
  return s == t;
}

int
f8 (int x, int y)
{
  int s = (x >= 0);
  int t = (y < 0);
  return s == t;
}
