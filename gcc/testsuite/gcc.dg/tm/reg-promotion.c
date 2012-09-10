/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O2 -fdump-tree-lim1" } */

/* Test that `count' is not written to unless p->data>0.  */

int count;

struct obj {
    int data;
    struct obj *next;
} *q;

void func()
{
  struct obj *p;
  __transaction_atomic {
    for (p = q; p; p = p->next)
      if (p->data > 0)
	count++;
  }
}

/* { dg-final { scan-tree-dump-times "Cannot hoist conditional load of count because it is in a transaction" 1 "lim1" } } */
/* { dg-final { cleanup-tree-dump "lim1" } } */
