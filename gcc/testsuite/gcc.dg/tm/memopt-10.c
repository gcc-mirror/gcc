/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O -fdump-tree-tmmark" } */

extern int something(void) __attribute__((transaction_safe));
extern void *malloc (__SIZE_TYPE__) __attribute__((malloc,transaction_safe));

int f()
{
  int *p;

  p = malloc (sizeof (*p) * 100);

  __transaction_atomic {
    /* p[5] is thread private, but not transaction local since the
       malloc is outside of the transaction.  We can use the logging
       functions for this.  */
    p[5] = 123;

    if (something())
      __transaction_cancel;
  }
  return p[5];
}

/* { dg-final { scan-tree-dump-times "ITM_LU" 0 "tmmark" } } */
/* { dg-final { scan-tree-dump-times "ITM_WU" 0 "tmmark" } } */
/* { dg-final { scan-tree-dump-times "int tm_save" 1 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
