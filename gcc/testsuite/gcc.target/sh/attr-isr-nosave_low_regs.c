/* A call will clobber all call-saved registers.
   If #pragma nosave_low_regs is specified, do not save/restore r0..r7.
   (On SH3* and SH4* r0..r7 are banked)
   Call-saved registers r8..r13 also don't need to be restored.
   To test that we look for register push insns such as 'mov.l r0,@-r15'.  */
/* { dg-do compile { target { { banked_r0r7_isr } && nonpic } } }  */
/* { dg-options "-O" }  */
/* { dg-final { scan-assembler-times "rte" 1 } }  */
/* { dg-final { scan-assembler-not "mov.l\tr\[0-9\],@-r15" } }  */
/* { dg-final { scan-assembler-not "mov.l\tr1\[0-4\],@-r15" } }  */
/* { dg-final { scan-assembler-times "macl" 2 } }  */

extern void bar (void);

void
foo (void)
{
}

#pragma interrupt
void
( __attribute__ ((nosave_low_regs)) isr) (void)
{
  bar ();
}

void
delay (int a)
{
}
