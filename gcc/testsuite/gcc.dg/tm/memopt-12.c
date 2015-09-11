/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O -fdump-tree-tmmark" } */

extern int test(void) __attribute__((transaction_safe));
extern int something (void);
extern void *malloc (__SIZE_TYPE__) __attribute__((malloc,transaction_safe));

struct large { int foo[500]; };

int f()
{
  int *p1, *p2, *p3;

  p1 = malloc (sizeof (*p1)*5000);
  __transaction_atomic {
    *p1 = 0;

    p2 = malloc (sizeof (*p2)*6000);
    *p2 = 1;

    /* p3 = PHI (p1, p2) */
    if (test())
      p3 = p1;
    else
      p3 = p2;

    /* Since both p1 and p2 are thread-private, we can inherit the
       logging already done.  No ITM_W* instrumentation necessary.  */
    *p3 = 555;
  }
  return p3[something()];
}

/* { dg-final { scan-tree-dump-times "ITM_WU" 0 "tmmark" } } */
