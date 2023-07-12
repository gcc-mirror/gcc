/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512f -mno-avx512vl -mprefer-vector-width=512 -O2" } */
/* { dg-final { scan-assembler "vbroadcastss\[^\n\]*%xmm17, *%zmm" } } */

typedef float __attribute__ ((vector_size (16))) v4sf;

v4sf bcst (float f)
{
  register float x asm ("xmm17") = f;

  asm ("" : "+v" (x));
  return (v4sf) {x, x, x, x};
}
