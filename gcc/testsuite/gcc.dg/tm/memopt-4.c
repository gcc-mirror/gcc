/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O -fdump-tree-tmedge" } */

/* Test thread-local memory optimizations: save/restore pairs.  */

struct large { int x[100]; };
struct large bark();
extern int test (void) __attribute__((transaction_safe));
extern int readint (void);

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

/* { dg-final { scan-tree-dump-times "tm_save.\[0-9_\]+ = lala.x\\\[55\\\]" 1 "tmedge" } } */
/* { dg-final { scan-tree-dump-times "lala.x\\\[55\\\] = tm_save" 1 "tmedge" } } */
