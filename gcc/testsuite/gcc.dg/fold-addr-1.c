/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

int bar(char p1, char p2)
{
  return &p1 == &p2;
}

/* { dg-final { scan-tree-dump "return 0;" "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
