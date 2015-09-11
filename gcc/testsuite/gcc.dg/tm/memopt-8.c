/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O -fdump-tree-tmmark" } */

extern int something(void) __attribute__((transaction_safe));
extern int escape(int *) __attribute__((transaction_safe));
extern void *malloc (__SIZE_TYPE__) __attribute__((malloc,transaction_safe));

int f()
{
  int *p;

  __transaction_atomic {
    p = malloc (sizeof (*p) * 100);
    escape (p);

    /* This should be instrumented because P escapes.  */
    p[5] = 123;

    if (something())
      __transaction_cancel;
  }
  return p[5];
}

/* { dg-final { scan-tree-dump-times "ITM_WU" 1 "tmmark" } } */
