/* { dg-do compile } */
/* { dg-require-effective-target archs }*/
/* { dg-options "-O0 -mll64 -mirq-ctrl-saved=r0-r9 -mrgf-banked-regs=4" } */

/* Check that on archs the 'firq' interrupt function type is
   available, these are the fast interrupts.  For fast interrupts,
   despite the use of 'irq-ctrl-saved', no registers are automatically
   saved on stack on entry to the function.  However, the cpu save via
   bank switch R0-R3.

   We also take the opportunity to check the use of the 'rtie' instruction
   at the end of the interrupt function.  */

void __attribute__ ((interrupt("firq")))
handler1 (void)
{
  asm (""
       :
       :
       : "r0", "r1", "r2", "r3", "r4",
         "r5", "r6", "r7", "r8", "r9");
}
/* { dg-final { scan-assembler-not "r0,\\\[sp" } } */
/* { dg-final { scan-assembler-not "push.*r0" } } */
/* { dg-final { scan-assembler-not "r1,\\\[sp" } } */
/* { dg-final { scan-assembler-not "r2,\\\[sp" } } */
/* { dg-final { scan-assembler-not "r3,\\\[sp" } } */
/* { dg-final { scan-assembler "st.*r4,\\\[sp" } } */
/* { dg-final { scan-assembler "st.*r6,\\\[sp,\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "st.*r8,\\\[sp,\[0-9\]+\\\]" } } */
/* { dg-final { scan-assembler "rtie" } } */
