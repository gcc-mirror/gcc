/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void foo();

void test5_1(int e)
{
  if ((e >> 31) & 64)
    foo();
}

typedef int myint;

void test5_2(myint e)
{
  if ((e >> 31) & 64)
    foo();
}

/* { dg-final { scan-tree-dump-times " < 0" 2 "optimized" } } */
