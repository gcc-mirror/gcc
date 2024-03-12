/* { dg-do assemble { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512" } */

extern double d;

void
foo1 (double x)
{
  register double xmm16 __asm ("xmm16") = x;
  asm volatile ("" : "+v" (xmm16));
  register double xmm17 __asm ("xmm17") = xmm16;
  asm volatile ("" : "+v" (xmm17));
  d = xmm17;
}
