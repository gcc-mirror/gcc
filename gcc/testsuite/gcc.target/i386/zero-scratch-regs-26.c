/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -msse2 -fzero-call-used-regs=all-arg" } */

int 
foo (int x)
{
  return x;
}

/* { dg-final { scan-assembler "xorl\[ \t\]+%edx, %edx" } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%ecx, %ecx" } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%esi, %esi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%edi, %edi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%r8d, %r8d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "xorl\[ \t\]+%r9d, %r9d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm0, %xmm0" } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm1, %xmm1" } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm2, %xmm2" } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm3, %xmm3" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm4, %xmm4" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm5, %xmm5" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm6, %xmm6" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "\[a-z\]*xor\[a-z\]*\[ \t\]+%xmm7, %xmm7" { target { ! ia32 } } } } */
