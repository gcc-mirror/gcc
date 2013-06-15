/* Check that no interrupt-specific register saves are generated.  */
/* { dg-do compile { target { { "sh*-*-*" } && nonpic } } }  */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } }  */
/* { dg-options "-O" }  */
/* { dg-final { scan-assembler-times "rte" 1 } }  */
/* { dg-final { scan-assembler-not "r\[0-7\]\[ \t,\]\[^\n\]*r15" } }  */
/* { dg-final { scan-assembler-not "@r15\[^\n\]*r\[0-7\]\n" } }  */
/* { dg-final { scan-assembler-not "r\[8-9\]" } }  */
/* { dg-final { scan-assembler-not "r1\[,0-3\]" } }  */
/* { dg-final { scan-assembler-not "macl" } }  */

extern void foo (void);

void
(__attribute__ ((trapa_handler)) isr) (void)
{
  foo ();
}
