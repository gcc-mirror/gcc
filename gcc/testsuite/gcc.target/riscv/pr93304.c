/* Verify the regrename won't rename registers to register which never used
   before.  */
/* { dg-do compile } */
/* { dg-options "-O -frename-registers" } */

static unsigned _t = 0;

void __attribute__ ((interrupt))
foo (void)
{
  _t++;
}

/* Register rename will try to use registers from the lower register
   regradless of the REG_ALLOC_ORDER.
   In theory, t0-t6 should not used in such small program if regrename
   not executed incorrectly, because a5-a0 has higher priority in
   REG_ALLOC_ORDER.  */
/* { dg-final { scan-assembler-not "t\[0-6\]" } } */
