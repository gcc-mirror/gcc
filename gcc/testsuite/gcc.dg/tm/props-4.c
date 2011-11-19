/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-tree-tmedge -fdump-tree-tmmark" } */

int a, b;

void __attribute((transaction_may_cancel_outer,noinline)) cancel1()
{
  __transaction_cancel [[outer]];
}

void
foo(void)
{
  __transaction_atomic [[outer]] {
    a = 2;
    __transaction_atomic {
      b = 2;
      cancel1();
    }
  }
}

/* { dg-final { scan-tree-dump-times " instrumentedCode" 1 "tmedge" } } */
/* { dg-final { scan-tree-dump-times "hasNoAbort" 0 "tmedge" } } */
/* { dg-final { scan-tree-dump-times "LABEL=<L0>" 1 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmedge" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
