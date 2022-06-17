/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fzero-call-used-regs=skip -fno-stack-protector" } */

extern int foo (int) __attribute__ ((zero_call_used_regs("used-gpr")));

int
foo (int x)
{
  return x;
}

/* { dg-final { scan-assembler-not "vzeroall" } } */
/* { dg-final { scan-assembler-not "%xmm" } } */
/* { dg-final { scan-assembler-not "xorl\[ \t\]+%" { target ia32 } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%edi, %edi" { target { ! ia32 } } } } */
