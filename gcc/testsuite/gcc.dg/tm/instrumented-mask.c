/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-tree-tmmark" } */

/* If we're sure to go irrevocable, as in the case below, do not pass
   PR_INSTRUMENTEDCODE to the run-time if there is nothing
   instrumented within the transaction.  */

int
main()
{
  __transaction_relaxed { __asm__(""); }
  return 0;
}

/* { dg-final { scan-tree-dump-times " instrumentedCode" 0 "tmmark" } } */
/* { dg-final { cleanup-tree-dump "tmmark" } } */
