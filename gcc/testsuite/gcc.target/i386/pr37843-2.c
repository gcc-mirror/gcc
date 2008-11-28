/* Test for stack alignment with sibcall optimization.  */
/* { dg-do compile { target nonpic } } */
/* { dg-options "-O2 -mpreferred-stack-boundary=6 -mincoming-stack-boundary=6" } */
/* { dg-final { scan-assembler-not "and\[lq\]?\[\\t \]*\\$-64,\[\\t \]*%\[re\]?sp" } } */
/* { dg-final { scan-assembler-not "call\[\\t \]*foo" } } */
/* { dg-final { scan-assembler "jmp\[\\t \]*foo" } } */

extern int foo (void);

int bar (void)
{
    return foo();
}
