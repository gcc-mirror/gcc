/* Test for stack alignment with sibcall optimization.  */
/* { dg-do compile { target nonpic } } */
/* { dg-options "-O2 -mpreferred-stack-boundary=6 -mincoming-stack-boundary=5" } */
/* { dg-final { scan-assembler "and\[lq\]?\[\\t \]*\\$-64,\[\\t \]*%\[re\]?sp" } } */
/* { dg-final { scan-assembler "call\[\\t \]*foo" } } */
/* { dg-final { scan-assembler-not "jmp\[\\t \]*foo" } } */

extern int foo (void);

int bar (void)
{
    return foo();
}
