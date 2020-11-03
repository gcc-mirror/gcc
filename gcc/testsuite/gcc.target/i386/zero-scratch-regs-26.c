/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -msse2 -fzero-call-used-regs=all-arg" } */

int 
foo (int x)
{
  return x;
}

/* { dg-final { scan-assembler "xorl\[ \t\]+%edx, %edx" } } */
/* { dg-final { scan-assembler "movl\[ \t\]+%edx, %ecx" } } */
/* { dg-final { scan-assembler "movl\[ \t\]+%edx, %esi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]+%edx, %edi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]+%edx, %r8d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movl\[ \t\]+%edx, %r9d" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "pxor\[ \t\]+%xmm0, %xmm0" } } */
/* { dg-final { scan-assembler "movaps\[ \t\]+%xmm0, %xmm1" } } */
/* { dg-final { scan-assembler "movaps\[ \t\]+%xmm0, %xmm2" } } */
/* { dg-final { scan-assembler "movaps\[ \t\]+%xmm0, %xmm3" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movaps\[ \t\]+%xmm0, %xmm4" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movaps\[ \t\]+%xmm0, %xmm5" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movaps\[ \t\]+%xmm0, %xmm6" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movaps\[ \t\]+%xmm0, %xmm7" { target { ! ia32 } } } } */
