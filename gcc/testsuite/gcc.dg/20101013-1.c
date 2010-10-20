/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo(int a)
{
  int x = (a & (~15)) / 16;
  return x;
}

/* { dg-final { scan-tree-dump ">>" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
