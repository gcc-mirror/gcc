/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "x_\[0-9]+\\\(D\\\) & y_\[0-9]+\\\(D\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump-not "x_\[0-9]+\\\(D\\\) \\| y_\[0-9]+\\\(D\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump-times "x_\[0-9]+\\\(D\\\) \\^ y_\[0-9]+\\\(D\\\);" 5 "optimized" } } */
/* { dg-final { scan-tree-dump-times "~_\[0-9]\+" 5 "optimized" } } */

int
a (int x, int y)
{
  return (x & y) - (x | y) - 1;
}

int
b (int x, int y)
{
  return (x & y) - 1 - (x | y);
}

int
c (int x, int y)
{
  return (x & y) - ((x | y) + 1);
}

int
d (int x, int y)
{
  return (x & y) - (1 + (x | y));
}

int
e (int x, int y)
{
  return (unsigned) ((x & y) - (x | y)) + -1u;
}
