/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fzero-call-used-regs=all-arg" } */

int 
foo (int x)
{
  return x;
}

/* { dg-final { scan-assembler "xorl\[ \t\]*%edx, %edx" } } */
/* { dg-final { scan-assembler "movl\[ \t\]*%edx, %ecx" } } */
/* { dg-final { scan-assembler "movl\[ \t\]*%edx, %esi" } } */
/* { dg-final { scan-assembler "movl\[ \t\]*%edx, %edi" } } */
/* { dg-final { scan-assembler "movl\[ \t\]*%edx, %r8d" } } */
/* { dg-final { scan-assembler "movl\[ \t\]*%edx, %r9d" } } */
/* { dg-final { scan-assembler "pxor\[ \t\]*%xmm0, %xmm0" } } */
/* { dg-final { scan-assembler "movaps\[ \t\]*%xmm0, %xmm1" } } */
/* { dg-final { scan-assembler "movaps\[ \t\]*%xmm0, %xmm2" } } */
/* { dg-final { scan-assembler "movaps\[ \t\]*%xmm0, %xmm3" } } */
/* { dg-final { scan-assembler "movaps\[ \t\]*%xmm0, %xmm4" } } */
/* { dg-final { scan-assembler "movaps\[ \t\]*%xmm0, %xmm5" } } */
/* { dg-final { scan-assembler "movaps\[ \t\]*%xmm0, %xmm6" } } */
/* { dg-final { scan-assembler "movaps\[ \t\]*%xmm0, %xmm7" } } */
