/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1-details" } */

void *foo (int *p)
{
  void *q;
  /* We should be able to DSE this store (p may point to errno).  */
  *p = 0;
  q = __builtin_malloc (4);
  *p = 0;
  return q;
}

int j;
void bar (int *i)
{
  /* This store is dead as well.  */
  j = 1;
  *i = 0;
  j = 2;
}

/* { dg-final { scan-tree-dump-times "Deleted dead store" 2 "dse1" } } */
