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
   In theory, t2 should not used in such small program if regrename
   not executed incorrectly, because t0-a2 should be enough.  */
/* { dg-final { scan-assembler-not "t2" } } */
