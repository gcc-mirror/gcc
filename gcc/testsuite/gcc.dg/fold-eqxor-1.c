/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

int test1(int a, int b)
{
  return (a ^ b) == 0;
}

int test2(int c, int d)
{
  return (c ^ d) != 0;
}

unsigned int test3(unsigned int e, unsigned int f)
{
  return (e ^ f) == 0;
}

unsigned int test4(unsigned int g, unsigned int h)
{
  return (g ^ h) != 0;
}

/* { dg-final { scan-tree-dump-times "a == b" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "c != d" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "e == f" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "g != h" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
