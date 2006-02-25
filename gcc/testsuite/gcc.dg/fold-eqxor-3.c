/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

int test1(int a, int b)
{
  return (a ^ 2) == 2;
}

int test2(int c, int d)
{
  return (c ^ 4) != 4;
}

int test3(int e, int f)
{
  return (e ^ 2) == 6;
}

int test4(int g, int h)
{
  return (g ^ 6) != 4;
}

/* { dg-final { scan-tree-dump-times "a == 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "c != 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "e == 4" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "g != 2" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
