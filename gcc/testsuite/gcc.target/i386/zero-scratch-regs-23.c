/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fzero-call-used-regs=all -march=corei7 -mavx512f" } */

void
foo (void)
{
}

/* { dg-final { scan-assembler "vzeroall" } } */
/* { dg-final { scan-assembler-times "fldz" 8 } } */
/* { dg-final { scan-assembler-times "fstp\[ \t\]+%st\\(0\\)" 8 } } */
/* { dg-final { scan-assembler-not "%xmm" } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%eax, %eax" } } */
/* { dg-final { scan-assembler "movl\[ \t\]+%eax, %edx" } } */
/* { dg-final { scan-assembler "movl\[ \t\]+%eax, %ecx" } } */
/* { dg-final { scan-assembler "movl\[ \t\]+%eax, %esi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]+%eax, %edi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]+%eax, %r8d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]+%eax, %r9d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]+%eax, %r10d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]+%eax, %r11d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kxorw\[ \t\]+%k0, %k0, %k0" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kmovw\[ \t\]+%k0, %k1" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kmovw\[ \t\]+%k0, %k2" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kmovw\[ \t\]+%k0, %k3" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kmovw\[ \t\]+%k0, %k4" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kmovw\[ \t\]+%k0, %k5" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kmovw\[ \t\]+%k0, %k6" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kmovw\[ \t\]+%k0, %k7" { target { ! ia32 } } } } */
