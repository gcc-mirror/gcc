/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512" } */

extern int i;

int
foo1 (void)
{
  register int xmm16 __asm ("xmm16") = i;
  asm volatile ("" : "+v" (xmm16));
  register int xmm17 __asm ("xmm17") = xmm16;
  asm volatile ("" : "+v" (xmm17));
  return xmm17;
}

/* { dg-final { scan-assembler-times "vmovdqa32\[^\n\r]*xmm1\[67]\[^\n\r]*xmm1\[67]" 1 } } */
/* { dg-final { scan-assembler-not "%zmm\[0-9\]+" } } */
