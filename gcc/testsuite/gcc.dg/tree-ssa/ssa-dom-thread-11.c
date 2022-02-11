/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dom2-details --param logical-op-non-short-circuit=1 -fdisable-tree-thread1 -fdisable-tree-thread2 -fdisable-tree-threadfull1" } */

static int *bb_ticks;
extern void frob (void);
void
mark_target_live_regs (int b, int block, int bb_tick)
{
  if (b == block && b != -1 && bb_tick == bb_ticks[b])
      return;
  if (b != -1)
    frob ();
}

/* When the first two conditionals in the first IF are true, but
   the third conditional is false, then there's a jump threading
   opportunity to bypass the second IF statement.  */
/* { dg-final { scan-tree-dump-times "Threaded" 1 "dom2"} } */
