/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "x_\[0-9]+\\\(D\\\) & y_\[0-9]+\\\(D\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump-not "x_\[0-9]+\\\(D\\\) \\| y_\[0-9]+\\\(D\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump-times "x_\[0-9]+\\\(D\\\) \\^ y_\[0-9]+\\\(D\\\);" 8 "optimized" } } */
/* { dg-final { scan-tree-dump-times "~_\[0-9]\+" 8 "optimized" } } */

int
a (int x, int y)
{
  unsigned t = x & y;
  unsigned tt = x | y;
  t = t - tt;
  return t + -1;
}

int
a1 (int x, int y)
{
  int t = x & y;
  int tt = x | y;
  unsigned t1 = t - tt;
  return t1 + -1;
}

int
b (int x, int y)
{
  unsigned t = x & y;
  unsigned tt = x | y;
  t = t - 1;
  return t - tt;
}

int
b1 (int x, int y)
{
  int t = x & y;
  int tt = x | y;
  unsigned t1 = t - 1;
  return t1 - tt;
}

int
c (int x, int y)
{
  unsigned t = x & y;
  unsigned tt = x | y;
  tt = tt + 1;
  return t - tt;
}

int
c1 (int x, int y)
{
  int t = x & y;
  int tt = x | y;
  unsigned tt1 = tt + 1;
  return t - tt1;
}

int
d (int x, int y)
{
  unsigned t = x & y;
  unsigned tt = x | y;
  tt = tt + 1;
  return t - tt;
}

int
d1 (int x, int y)
{
  int t = x & y;
  int tt = x | y;
  unsigned tt1 = tt + 1;
  return t - tt1;
}
