/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int foo(int a)
{
  int t;
  *(volatile int *)&t = a;
}

/* { dg-final { scan-tree-dump "={v}" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
