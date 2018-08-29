/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

int test(int x)
{
  return (x+x)/x;
}

/* { dg-final { scan-tree-dump "return 2;" "original" } } */
