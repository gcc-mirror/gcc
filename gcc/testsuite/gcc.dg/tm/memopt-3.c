/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O -fdump-tree-tmmark" } */

struct large { int x[100]; };
extern int test(void) __attribute__((transaction_safe));

int f()
{
  int i = readint();
  struct large lala = { 0 };
  __transaction_atomic {
    lala.x[i] = 666;
    if (test())
      __transaction_cancel;
  }
  return lala.x[0];
}

/* { dg-final { scan-tree-dump-times "logging: lala.x\\\[i_1\\\]" 1 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
