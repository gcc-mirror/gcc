/* { dg-do compile } */
/* { dg-require-effective-target archs }*/
/* { dg-options "-O2 -mll64 -mrgf-banked-regs=32" } */

/* Check if we have any register saved on stack.  */

void __attribute__ ((interrupt("firq")))
handler1 (void)
{
  asm volatile (""
		:
		:
		: "r0", "r1", "r2", "r3", "r4",
		  "r5", "r6", "r7", "r8", "r9",
		  "r10", "r11", "r12", "r13", "r14",
		  "r15", "r16", "r17", "r18", "r19",
		  "r20", "r21", "r22", "r23", "r24",
		  "r25", "fp");
}
/* { dg-final { scan-assembler-not "(s|l)(t|d)d.*r\[0-9\]+,\\\[sp,\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "mov_s.*fp,sp" } } */
