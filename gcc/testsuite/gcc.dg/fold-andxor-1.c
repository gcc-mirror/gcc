/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

int test1(int a, int b)
{
  return (a ^ b) & a;
}

int test2(int c, int d)
{
  return (c ^ d) & d;
}

int test3(int e, int f)
{
  return e & (e ^ f);
}

int test4(int g, int h)
{
  return g & (h ^ g);
}

/* { dg-final { scan-tree-dump-times "~b \& a" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "~c \& d" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "~f \& e" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "~h \& g" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */

