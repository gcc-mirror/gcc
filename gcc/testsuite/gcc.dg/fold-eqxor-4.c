/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

int test1(int a)
{
  return (a ^ 6) == 2;
}

int test2(int b, int c, int d)
{
  return (b ^ d) == (c ^ d);
}

int test3(int e, int f)
{
  return (e ^ 6) == (f ^ 4);
}

/* { dg-final { scan-tree-dump-times "a == 4" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "b == c" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "e \\^ 2" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
