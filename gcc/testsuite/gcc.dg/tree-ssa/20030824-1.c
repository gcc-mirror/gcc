/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

struct A
{
  int a,b;
};

int foo (int x, int y)
{
  int i, j;
  struct A a,b;

  a.a = x;
  b.b = y;
  j = a.a;
  i = b.b;
  return i + j;
}

/* The addition should be optimized into 'y+x'.  */
/* { dg-final { scan-tree-dump-times "\[xy\]_..D. \\+ \[xy]_..D." 1 "optimized"} } */
