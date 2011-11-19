/* Test for stack alignment with sibcall optimization.  */
/* { dg-do compile { target { ia32 && nonpic } } } */
/* { dg-options "-O2 -msse2 -mpreferred-stack-boundary=4 -mstackrealign" } */
/* { dg-require-effective-target sse2 } */
/* { dg-final { scan-assembler-not "andl\[\\t \]*\\$-16,\[\\t \]*%\[re\]?sp" } } */
/* { dg-final { scan-assembler-not "call\[\\t \]*_?foo" } } */
/* { dg-final { scan-assembler "jmp\[\\t \]*_?foo" } } */

extern int foo (void);

int bar (void)
{
    return foo();
}
