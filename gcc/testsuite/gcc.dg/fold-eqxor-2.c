/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

int test1(int a, int b)
{
  return (a ^ b) == a;
}

int test2(int c, int d)
{
  return (c ^ d) != c;
}

int test3(int e, int f)
{
  return (e ^ f) == f;
}

int test4(int g, int h)
{
  return (g ^ h) != h;
}

/* { dg-final { scan-tree-dump-times "b == 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "d != 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "e == 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "g != 0" 1 "original" } } */
