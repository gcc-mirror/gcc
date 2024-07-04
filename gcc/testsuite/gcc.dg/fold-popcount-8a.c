/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo1(unsigned int x, unsigned int y)
{
  int t = __builtin_popcount (x&y);
  int t1 = __builtin_popcount (x|y);
  return t + t1;
}

int foo2(unsigned int x, unsigned int y)
{
  int t1 = __builtin_popcount (x|y);
  int t = __builtin_popcount (x&y);
  return t + t1;
}

int foo3(unsigned int y, unsigned int x)
{
  int t = __builtin_popcount (x&y);
  int t1 = __builtin_popcount (x|y);
  return t + t1;
}

int foo4(unsigned int y, unsigned int x)
{
  int t1 = __builtin_popcount (x|y);
  int t = __builtin_popcount (x&y);
  return t + t1;
}

/* { dg-final { scan-tree-dump-not " & " "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\| " "optimized" } } */
