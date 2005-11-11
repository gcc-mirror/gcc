/* As the invalid insn in this test got as far as to the target output
   code and was "near enough" to output invalid assembly-code, we need
   to pass it through the assembler as well.
   { dg-do assemble } */

int
f (short *a, char *y)
{
  __asm__ ("" : : :
#ifndef __PIC__
	   "r0",
#endif
	   "r1", "r2", "r3", "r4", "r5", "r6", "r7",
	   /* Register R8 is frame-pointer, and we don't have a means
	      to not clobber it for the test-runs that don't eliminate
	      it.  But that's ok; we have enough general-register
	      pressure to repeat the bug without that.  */
	   "r9", "r10", "r11", "r12", "r13");
  return y[*a];
}
