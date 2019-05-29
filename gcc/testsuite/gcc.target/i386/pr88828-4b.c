/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler-times "vpermilps" 1 } } */
/* { dg-final { scan-assembler-times "vmovss" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpinsrd" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-not "vmovss" { target ia32 } } } */
/* { dg-final { scan-assembler-not "vshufps" } } */
/* { dg-final { scan-assembler-not "vmovaps" } } */
/* { dg-final { scan-assembler-not "vmovlhps" } } */
/* { dg-final { scan-assembler-not "vunpcklps" } } */

typedef float __v4sf __attribute__ ((__vector_size__ (16)));

__attribute__((noinline, noclone))
__v4sf
foo (__v4sf x, float f)
{
  __v4sf y = { x[0], x[2], x[3], x[1] };
  y[0] = f;
  return y;
}
