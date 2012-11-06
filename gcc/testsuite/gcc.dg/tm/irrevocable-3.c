/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-tree-tmmark" } */

extern void bar(void) __attribute__((transaction_callable));

foo()
{
	__transaction_relaxed {
		bar();
	}
}

/* { dg-final { scan-tree-dump-times "doesGoIrrevocable" 1 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
