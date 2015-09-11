/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

int test1(int a)
{
  return ~(unsigned int)a;
}

unsigned int test2(unsigned int b)
{
  return ~(int)b;
}

/* { dg-final { scan-tree-dump-times "~a" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "~b" 1 "original" } } */

