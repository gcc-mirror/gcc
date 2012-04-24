/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int f(int y, int x)
{
  int a = x | y;
  return a & x;
}
int f1(int y, int x)
{
  int a = y | x;
  return a & x;
}
int f2(int y, int x)
{
  int a = x | y;
  return x & a;
}
int f3(int y, int x)
{
  int a = x | y;
  return x & a;
}
int f4(int y, int x)
{
  int a = x & y;
  return a | x;
}
int f5(int y, int x)
{
  int a = y & x;
  return a | x;
}
int f6(int y, int x)
{
  int a = x & y;
  return x | a;
}
int f7(int y, int x)
{
  int a = x & y;
  return x | a;
}
/* These all should be optimized to just return x; */


/* { dg-final { scan-tree-dump-times "\\\|" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\&" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return x_..D.;" 8 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
