/* { dg-do compile } */
/* { dg-require-effective-target archs }*/
/* { dg-options "-O2 -mll64 -mrgf-banked-regs=8" } */

/* Check if R4 to R11 and R16-R27 are correctly saved on stack.  */

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
/* { dg-final { scan-assembler-not "r0,\\\[sp" } } */
/* { dg-final { scan-assembler-not "push.*r0" } } */
/* { dg-final { scan-assembler-not "r1,\\\[sp" } } */
/* { dg-final { scan-assembler-not "r2,\\\[sp" } } */
/* { dg-final { scan-assembler-not "r3,\\\[sp" } } */
/* { dg-final { scan-assembler-not "r12,\\\[sp" } } */
/* { dg-final { scan-assembler-not "r13,\\\[sp" } } */
/* { dg-final { scan-assembler-not "r14,\\\[sp" } } */
/* { dg-final { scan-assembler-not "r15,\\\[sp" } } */

/* { dg-final { scan-assembler-times "r4,\\\[sp" 2 } } */
/* { dg-final { scan-assembler-times "r6,\\\[sp,\[0-9\]+\\\]" 2 } } */
/* { dg-final { scan-assembler-times "r8,\\\[sp,\[0-9\]+\\\]" 2 } } */
/* { dg-final { scan-assembler-times "r10,\\\[sp,\[0-9\]+\\\]" 2 } } */
/* { dg-final { scan-assembler-times "r16,\\\[sp,\[0-9\]+\\\]" 2 } } */
/* { dg-final { scan-assembler-times "r18,\\\[sp,\[0-9\]+\\\]" 2 } } */
/* { dg-final { scan-assembler-times "r20,\\\[sp,\[0-9\]+\\\]" 2 } } */
/* { dg-final { scan-assembler-times "r24,\\\[sp,\[0-9\]+\\\]" 2 } } */
/* { dg-final { scan-assembler-times "fp,\\\[sp," 2 } } */

/* { dg-final { scan-assembler "rtie" } } */
