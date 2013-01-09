/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-original" } */

int foo (int i)
{
  return (i + 2) - (i + 1);
}
int bar (int i)
{
  return (i + 2) + ~i;
}

/* { dg-final { scan-tree-dump "return 1;" "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
