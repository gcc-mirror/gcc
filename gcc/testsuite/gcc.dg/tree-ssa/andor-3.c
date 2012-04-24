/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int f(int y, int x)
{
  return x & ((~x) | y);
}
int f1(int y, int x)
{
  return x & (y | (~x));
}
int f2(int y, int x)
{
  return ((~x) | y) & x;
}
int f3(int y, int x)
{
  return (y | (~x)) & x;
}


/* { dg-final { scan-tree-dump-times "~x" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "x_..D. \& y_..D." 4 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
