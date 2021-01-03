/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fzero-call-used-regs=skip -march=corei7" } */

__attribute__ ((zero_call_used_regs("used")))
float
foo (float z, float y, float x)
{
  return x + y;
}

/* { dg-final { scan-assembler-not "vzeroall" } } */
/* { dg-final { scan-assembler "pxor\[ \t\]+%xmm1, %xmm1" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "movaps\[ \t\]+%xmm1, %xmm2" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "xorl\[ \t\]+%" } } */
