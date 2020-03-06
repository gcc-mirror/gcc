/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512 -mno-avx512vl" } */

extern __float128 d;

void
foo1 (__float128 x)
{
  register __float128 xmm16 __asm ("xmm16") = x; /* { dg-error "register specified for 'xmm16'" } */
  asm volatile ("" : "+v" (xmm16));
  d = xmm16;
}
