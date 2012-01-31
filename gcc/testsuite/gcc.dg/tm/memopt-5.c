/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O -fdump-tree-tmedge --param tm-max-aggregate-size=1" } */

/* Test thread-local memory optimizations: logging function.  */

struct large { int x[100]; };
struct large bark();
extern int test (void) __attribute__((transaction_safe));

int f()
{
  int i = readint();
  struct large lala = bark();
  __transaction_atomic {
    lala.x[55] = 666;
    if (test())
      __transaction_cancel;
  }
  return lala.x[i];
}

/* { dg-final { scan-tree-dump-times "ITM_LU\[0-9\] \\\(&lala.x\\\[55\\\]" 1 "tmedge" } } */
/* { dg-final { cleanup-tree-dump "tmedge" } } */
