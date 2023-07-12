/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512f -mno-avx512vl -mprefer-vector-width=512 -O2" } */
/* { dg-final { scan-assembler "vpbroadcastd\[^\n\]*%xmm17, *%zmm" } } */

typedef int __attribute__ ((vector_size (16))) v4si;

v4si bcst (int i)
{
  register int x asm ("xmm17") = i;

  asm ("" : "+v" (x));
  return (v4si) {x, x, x, x};
}
