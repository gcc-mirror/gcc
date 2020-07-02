/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "x_\[0-9]+\\\(D\\\) & y_\[0-9]+\\\(D\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump-not "x_\[0-9]+\\\(D\\\) \\| y_\[0-9]+\\\(D\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump-times "x_\[0-9]+\\\(D\\\) \\^ y_\[0-9]+\\\(D\\\);" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "~_\[0-9]\+" 4 "optimized" } } */

int
a (int x, int y)
{
  int t = x & y;
  int tt = x | y;
  t = t - tt;
  return t + -1;
}

int
b (int x, int y)
{
  int t = x & y;
  int tt = x | y;
  t = t - 1;
  return t - tt;
}

int
c (int x, int y)
{
  int t = x & y;
  int tt = x | y;
  tt = tt + 1;
  return t - tt;
}

int
d (int x, int y)
{
  int t = x & y;
  int tt = x | y;
  tt = tt + 1;
  return t - tt;
}
