/* Verify that the delay slot is not stuffed with register pop insns for
   interrupt handler function returns on SH1* and SH2* targets, where the
   rte insn uses the stack pointer.  */
/* { dg-do compile { target { stack_save_isr } } }  */
/* { dg-options "-O1" } */
/* { dg-final { scan-assembler-times "nop" 1 } } */

int test00 (int a, int b);

int __attribute__ ((interrupt_handler))
test01 (int a, int b, int c, int d)
{
  return test00 (a, b) + c;
}
