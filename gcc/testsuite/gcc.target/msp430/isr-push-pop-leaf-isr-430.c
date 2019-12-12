/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=msp430x*" "-mlarge" } { "" } } */
/* { dg-options "-mcpu=msp430" } */
/* { dg-final { scan-assembler "PUSH\tR5" } } */
/* { dg-final { scan-assembler "PUSH\tR12" } } */
/* { dg-final { scan-assembler-not "PUSH\tR4" } } */
/* { dg-final { scan-assembler-not "PUSH\tR11" } } */

/* To check that the compiler doesn't blindly save all regs, we omit R4 and R11
   from the trashing.  */
#define TRASH_REGS_LITE				\
  __asm__ ("mov #0xFFFF, r5" : : : "R5");	\
  __asm__ ("mov #0xFFFF, r6" : : : "R6");	\
  __asm__ ("mov #0xFFFF, r7" : : : "R7");	\
  __asm__ ("mov #0xFFFF, r8" : : : "R8");	\
  __asm__ ("mov #0xFFFF, r9" : : : "R9");	\
  __asm__ ("mov #0xFFFF, r10" : : : "R10");	\
  __asm__ ("mov #0xFFFF, r12" : : : "R12");	\
  __asm__ ("mov #0xFFFF, r13" : : : "R13");	\
  __asm__ ("mov #0xFFFF, r14" : : : "R14");	\
  __asm__ ("mov #0xFFFF, r15" : : : "R15");

void __attribute__((interrupt))
isr_leaf (void)
{
  TRASH_REGS_LITE
}
