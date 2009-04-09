/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1 -fdump-tree-forwprop1" } */

int a[256];
int foo(int i)
{
  int *p = &a[0];
  return *(p + i);
}

/* { dg-final { scan-tree-dump "&a\\\[D\\\." "ccp1" } } */
/* { dg-final { scan-tree-dump "= a\\\[D\\\." "forwprop1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */
