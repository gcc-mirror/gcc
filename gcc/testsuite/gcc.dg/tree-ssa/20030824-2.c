/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

struct A
{
  int a,b;
};

int foo (int x, int y)
{
  int i, j;
  struct A a;

  a.a = x;
  a.b = y;
  j = a.a;
  i = a.b;
  return i + j;
}

/* This function should be optimized into 'return y+x'.  */
/* { dg-final { scan-tree-dump-times "return \[xy\] \\+ \[xy\]" 1 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
