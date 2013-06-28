/* A call will clobber all call-saved registers.
   If #pragma nosave_low_regs is specified, do not save/restore r0..r7.
   (On SH3* and SH4* r0..r7 are banked)
   One of these registers will also do fine to hold the function address.
   Call-saved registers r8..r13 also don't need to be restored.  */
/* { dg-do compile { target { { "sh*-*-*" } && nonpic } } }  */
/* { dg-skip-if "" { "sh*-*-*" } { "-m1*" "-m2*" "-m5*" } { "" } }  */
/* { dg-options "-O" }  */
/* { dg-final { scan-assembler-times "rte" 1 } }  */
/* { dg-final { scan-assembler-not "\[^f\]r\[0-9\]\[ \t\]*," } }  */
/* { dg-final { scan-assembler-not "\[^f\]r\[89\]" } }  */
/* { dg-final { scan-assembler-not "\[^f\]r1\[,0-3\]" } }  */
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
