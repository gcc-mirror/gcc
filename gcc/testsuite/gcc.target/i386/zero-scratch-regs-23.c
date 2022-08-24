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
/* { dg-final { scan-assembler "xorl\[ \t\]+%edx, %edx" } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%ecx, %ecx" } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%esi, %esi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%edi, %edi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%r8d, %r8d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%r9d, %r9d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%r10d, %r10d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%r11d, %r11d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kxorw\[ \t\]+%k0, %k0, %k0" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kxorw\[ \t\]+%k1, %k1, %k1" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kxorw\[ \t\]+%k2, %k2, %k2" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kxorw\[ \t\]+%k3, %k3, %k3" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kxorw\[ \t\]+%k4, %k4, %k4" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kxorw\[ \t\]+%k5, %k5, %k5" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kxorw\[ \t\]+%k6, %k6, %k6" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "kxorw\[ \t\]+%k7, %k7, %k7" { target { ! ia32 } } } } */
