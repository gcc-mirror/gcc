/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dominator-opts -fdump-tree-store_ccp" } */

int
foo (int *p)
{
  *p = 0;
  return *p;
}

/* The store to *p should be propagated to the return statement.  */
/* { dg-final { scan-tree-dump-times "return 0" 1 "store_ccp" } } */
/* { dg-final { cleanup-tree-dump "store_ccp" } } */
