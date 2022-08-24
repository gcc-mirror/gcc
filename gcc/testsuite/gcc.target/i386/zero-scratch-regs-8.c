/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fzero-call-used-regs=all-gpr" } */

int
foo (int x)
{
  return x;
}

/* { dg-final { scan-assembler-not "vzeroall" } } */
/* { dg-final { scan-assembler-not "%xmm" } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%edx, %edx" } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%ecx, %ecx" } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%esi, %esi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%edi, %edi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%r8d, %r8d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%r9d, %r9d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%r10d, %r10d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%r11d, %r11d" { target { ! ia32 } } } } */
