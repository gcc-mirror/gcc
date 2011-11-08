/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-tree-tmmark" } */

void orig(void);
void xyz(void) __attribute__((transaction_wrap (orig)));


foo()
{
	__transaction_relaxed {
		orig();
	}
}

/* { dg-final { scan-tree-dump-times "GTMA_MAY_ENTER_IRREVOCABLE" 1 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
