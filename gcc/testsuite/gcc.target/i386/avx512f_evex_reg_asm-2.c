/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-O2 -mavx512f -ffixed-xmm0 -ffixed-xmm1 -ffixed-xmm2 -ffixed-xmm3 -ffixed-xmm4 -ffixed-xmm5 -ffixed-xmm6 -ffixed-xmm7 -ffixed-xmm8 -ffixed-xmm9 -ffixed-xmm10 -ffixed-xmm11 -ffixed-xmm12 -ffixed-xmm13 -ffixed-xmm14 -ffixed-xmm15" } */
/* { dg-final { scan-assembler "vaddss\[ \\t\]+\[^\n\]*%xmm(1\[6-9\]|2\[0-9\]|3\[0-1\])\[^\{\]" } } */

volatile float a, b, c, d;

void foo()
{
  __asm__ __volatile__( "vaddss %1, %2, %3;" : "=v"(c) : "v"(a),"v"(b),"v"(d) );
}
