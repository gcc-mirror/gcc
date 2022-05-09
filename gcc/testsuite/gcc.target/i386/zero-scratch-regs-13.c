/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fzero-call-used-regs=all -march=corei7 -msse" } */

void
foo (void)
{
}

/* { dg-final { scan-assembler-not "vzeroall" } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm0, %xmm0" } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm1, %xmm1" } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm2, %xmm2" } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm3, %xmm3" } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm4, %xmm4" } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm5, %xmm5" } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm6, %xmm6" } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm7, %xmm7" } } */
/* { dg-final { scan-assembler "pxor\[ \t\]+%xmm8, %xmm8" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "pxor\[ \t\]+%xmm9, %xmm9" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "pxor\[ \t\]+%xmm10, %xmm10" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "pxor\[ \t\]+%xmm11, %xmm11" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "pxor\[ \t\]+%xmm12, %xmm12" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "pxor\[ \t\]+%xmm13, %xmm13" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "pxor\[ \t\]+%xmm14, %xmm14" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "pxor\[ \t\]+%xmm15, %xmm15" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%eax, %eax" } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%edx, %edx" } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%ecx, %ecx" } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%esi, %esi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%edi, %edi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%r8d, %r8d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%r9d, %r9d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%r10d, %r10d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%r11d, %r11d" { target { ! ia32 } } } } */
