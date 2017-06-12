/* { dg-do compile } */
/* { dg-require-effective-target archs }*/
/* { dg-options "-O2 -mrgf-banked-regs=16" } */

/* Check if blink is pushed on the stack or not.   */

extern void bar (void);

void __attribute__ ((interrupt("firq")))
handler1 (void)
{
  bar ();
}
/* { dg-final { scan-assembler-not "push.*blink" } } */
/* { dg-final { scan-assembler-not "pop.*blink" } } */
