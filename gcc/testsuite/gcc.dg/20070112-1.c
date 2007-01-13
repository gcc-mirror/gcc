/* We used to ICE on this testcase: the address of bad_spill_regs is
   only taken in a phi node, which we missed and cleared its TREE_ADDRESSABLE
   flag.  This gets revealed when the loop is completely unrolled, thus
   exposing the addresses as ordinary statements.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */


typedef unsigned HARD_REG_SET[2];
static HARD_REG_SET bad_spill_regs;

void
order_regs_for_reload ()
{
  register int i;

  register unsigned *scan_tp_ = bad_spill_regs;

  for (i = 0; i < 2; i++)
    *scan_tp_++ = 0;

  bad_spill_regs[0] |= 1;
}
