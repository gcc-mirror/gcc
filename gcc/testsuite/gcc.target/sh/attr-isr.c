/* The call will clobber r0..r7, which will need not be saved/restored, but
   not the call-saved registers r8..r14.  Check this by counting the register
   push insns.  */
/* { dg-do compile { target { { ! sh2a } && nonpic } } } */
/* { dg-options "-O" } */
/* { dg-final { scan-assembler-times "rte" 1} } */
/* { dg-final { scan-assembler-times "mov.l\tr\[0-7\],@-r15" 8 } }  */
/* { dg-final { scan-assembler-not "mov.l\tr\[89\],@-r15" } }  */
/* { dg-final { scan-assembler-not "mov.l\tr1\[0-4\],@-r15" } }  */

extern void foo ();

void
(__attribute ((interrupt_handler)) isr)()
{
  foo ();
}
