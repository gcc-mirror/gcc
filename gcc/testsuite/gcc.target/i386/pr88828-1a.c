/* { dg-do compile } */
/* { dg-options "-O2 -msse -mno-sse4" } */
/* { dg-final { scan-assembler "movss" } } */
/* { dg-final { scan-assembler-not "movaps" } } */
/* { dg-final { scan-assembler-not "movlhps" } } */
/* { dg-final { scan-assembler-not "unpcklps" } } */
/* { dg-final { scan-assembler-not "shufps" } } */

typedef float __v4sf __attribute__ ((__vector_size__ (16)));

__attribute__((noinline, noclone))
__v4sf
foo1 (__v4sf x, float f)
{
  __v4sf y = { f, x[1], x[2], x[3] };
  return y;
}
