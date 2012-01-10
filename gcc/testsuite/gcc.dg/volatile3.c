/* { dg-do compile } */
/* { dg-options "-fdump-tree-ssa" } */

volatile int *q;
void foo(int i)
{
  volatile int a[2];
  volatile int *p = &a[i];
  q = p;
}

/* { dg-final { scan-tree-dump-not "{v}" "ssa" } } */
/* { dg-final { cleanup-tree-dump "ssa" } } */
