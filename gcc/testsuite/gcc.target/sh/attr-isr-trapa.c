/* Check that no interrupt-specific register saves are generated.  */
/* { dg-do compile { target { { "sh*-*-*" } && nonpic } } }  */
/* { dg-options "-O" }  */
/* { dg-final { scan-assembler-times "rte" 1 } }  */
/* { dg-final { scan-assembler-not "mov.l\tr\[0-9\],@-r15" } }  */
/* { dg-final { scan-assembler-not "mov.l\tr1\[0-4\],@-r15" } }  */
/* { dg-final { scan-assembler-not "macl" } }  */

extern void foo (void);

void
(__attribute__ ((trapa_handler)) isr) (void)
{
  foo ();
}
