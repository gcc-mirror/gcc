/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O -fdump-tree-tmmark" } */

extern int something(void) __attribute__((transaction_safe));
extern void *malloc (__SIZE_TYPE__) __attribute__((malloc,transaction_safe));

struct large { int foo[500]; };

int f()
{
  int *p;
  struct large *lp;

  __transaction_atomic {
    p = malloc (sizeof (*p) * 100);
    lp = malloc (sizeof (*lp) * 100);

    /* No instrumentation necessary; P and LP are transaction local.  */
    p[5] = 123;
    lp->foo[66] = 123;

    if (something())
      __transaction_cancel;
  }
  return p[5];
}

/* { dg-final { scan-tree-dump-times "ITM_WU" 0 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
