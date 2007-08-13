/* { dg-do compile { target { { sh-*-* sh[1234ble]*-*-* } && nonpic } } } */
/* { dg-options "-O" } */
extern void foo ();

void
(__attribute ((interrupt_handler)) isr)()
{
  foo ();
}

/* { dg-final { scan-assembler-times "rte" 1} } */
/* The call will clobber r0..r7, which will need not be saved/restored.
   One of these registers will do fine to hold the function address,
   hence the all-saved registers r8..r13 don't need to be restored.  */
/* { dg-final { scan-assembler-times "r15\[+\],\[ \t\]*r\[0-9\]\[ \t\]*\n" 8 } } */
/* { dg-final { scan-assembler-times "\[^f\]r\[0-9\]\[ \t\]*," 8 } } */
/* { dg-final { scan-assembler-not "\[^f\]r1\[0-3\]" } } */
/* { dg-final { scan-assembler-times "macl" 2} } */
/* { dg-final { scan-assembler-not "rte.*\n.*r15\[+\],r\[0-7\]\n" } } */
