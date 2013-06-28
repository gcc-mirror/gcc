/* Test for stack alignment with sibcall optimization.  */
/* { dg-do compile { target { { *-*-linux* *-*-gnu* } && ia32 } } } */
/* { dg-options "-O2 -mpreferred-stack-boundary=4 -mincoming-stack-boundary=2" } */
/* { dg-final { scan-assembler "andl\[\\t \]*\\$-16,\[\\t \]*%\[re\]?sp" } } */
/* { dg-final { scan-assembler "call\[\\t \]*foo" } } */
/* { dg-final { scan-assembler-not "jmp\[\\t \]*foo" } } */

extern int foo (void);

int
main ()
{
  return foo ();
}
