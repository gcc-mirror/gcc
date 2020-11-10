/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fzero-call-used-regs=used -march=corei7" } */

float
foo (float z, float y, float x)
{
  return x;
}

/* { dg-final { scan-assembler-not "vzeroall" } } */
/* { dg-final { scan-assembler "pxor\[ \t\]+%xmm2, %xmm2" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "xorl\[ \t\]+%" } } */
