/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512" } */

extern __float128 d;

void
foo1 (__float128 x)
{
  register __float128 xmm16 __asm ("xmm16") = x;
  asm volatile ("" : "+v" (xmm16));
  register __float128 xmm17 __asm ("xmm17") = xmm16;
  asm volatile ("" : "+v" (xmm17));
  d = xmm17;
}

/* { dg-final { scan-assembler-not "%zmm\[0-9\]+" } } */
