/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512" } */

extern float d;

void
foo1 (float x)
{
  register float xmm16 __asm ("xmm16") = x;
  asm volatile ("" : "+v" (xmm16));
  register float xmm17 __asm ("xmm17") = xmm16;
  asm volatile ("" : "+v" (xmm17));
  d = xmm17;
}

/* { dg-final { scan-assembler-not "%zmm\[0-9\]+" } } */
