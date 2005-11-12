/* Check that we don't get unnecessary insns due to reload using more
   insns than needed due to reloading of more locations than
   needed.  */
/* { dg-options -O2 } */
/* { dg-final { scan-assembler-not "movs.w" } } */
/* { dg-final { scan-assembler-not "move.w" } } */

/* As torture/pr24750-2.c, except we need to clobber R8 for thorough
   testing and know we can do, since we replace the frame-pointer.  */

int
f (short *a, char *y)
{
  __asm__ ("" : : :
#ifndef __PIC__
	   "r0",
#endif
	   "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8",
	   "r9", "r10", "r11", "r12", "r13");
  return y[*a];
}
