/* Verify that if IP is saved to ensure stack alignment, we don't load
   it into sp.  */
/* { dg-do compile { target arm*-*-* strongarm*-*-* xscale*-*-*} } */
/* { dg-options "-O -mno-apcs-frame -mcpu=iwmmxt -mabi=iwmmxt" } */
/* { dg-final { global compiler_flags; if ![string match "*-mthumb *" $compiler_flags] { scan-assembler "ldmfd\[ 	]sp!.*ip,\[ ]*pc" } } } */

/* This function uses all the call-saved registers, namely r4, r5, r6,
   r7, r8, r9, sl, fp.  Since we also save lr, that leaves an odd
   number of registers, and the compiler will push ip to align the
   stack.  Make sure that we restore ip into ip, not into sp as is
   done when using a frame pointer.  The -mno-apcs-frame option
   permits the frame pointer to be used as an ordinary register.  */

void
foo(void)
{
  __asm volatile ("" : : :
		  "r4", "r5", "r6", "r7", "r8", "r9", "sl", "fp", "lr");
}
