/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

int a[256];
int foo(int i)
{
  return (a + 1)[i];
}

/* { dg-final { scan-tree-dump "=.*&a\\\]\\\[D\\\." "forwprop1" } } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */
