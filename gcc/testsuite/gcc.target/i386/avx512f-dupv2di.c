/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512f -mno-avx512vl -mprefer-vector-width=512 -O2" } */
/* { dg-final { scan-assembler-not "vmovddup\[^\n\]*%xmm16" } } */

typedef long long __attribute__ ((vector_size (16))) v2di;

v2di bcst (long long ll)
{
  register long long x asm ("xmm16") = ll;

  asm ("" : "+v" (x));
  return (v2di) {x, x};
}
