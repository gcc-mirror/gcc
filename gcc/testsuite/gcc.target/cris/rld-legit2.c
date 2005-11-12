/* A variant of rld-legit1.c only for full code coverage of the
   initial version of cris_reload_address_legitimized.  */
/* { dg-options -O2 } */

short *
g (short *a, char *y)
{
  __asm__ ("" : : :
#ifndef __PIC__
	   "r0",
#endif
	   "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8",
	   "r9", "r10", "r11", "r12", "r13");
  y[*a++] = 0;
  return a;
}
