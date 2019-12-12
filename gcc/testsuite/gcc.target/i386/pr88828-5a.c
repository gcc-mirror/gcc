/* { dg-do compile } */
/* { dg-options "-O2 -msse -mno-sse4" } */
/* { dg-final { scan-assembler "movss" } } */
/* { dg-final { scan-assembler-times "shufps" 2 } } */
/* { dg-final { scan-assembler-times "movaps" 1 } } */
/* { dg-final { scan-assembler-not "movlhps" } } */
/* { dg-final { scan-assembler-not "unpcklps" } } */

typedef float __v4sf __attribute__ ((__vector_size__ (16)));

__attribute__((noinline, noclone))
__v4sf
foo (__v4sf x, float f)
{
  __v4sf y = { x[0], x[2], x[3], x[0] };
  y[3] = f;
  return y;
}
