/* Test for stack alignment with sibcall optimization.  */
/* { dg-do compile { target { *-*-linux* && ilp32 } } } */
/* { dg-options "-O2 -std=gnu99 -mpreferred-stack-boundary=4 -mincoming-stack-boundary=2" } */
/* { dg-final { scan-assembler-not "andl\[\\t \]*\\$-16,\[\\t \]*%\[re\]?sp" } } */
/* { dg-final { scan-assembler-not "call\[\\t \]*foo" } } */
/* { dg-final { scan-assembler "jmp\[\\t \]*foo" } } */

extern int foo (_Decimal128);

int bar (_Decimal128 x)
{
    return foo(x);
}
