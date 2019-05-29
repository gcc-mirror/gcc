/* { dg-do compile } */
/* { dg-options "-O2 -msse -mno-sse4" } */
/* { dg-final { scan-assembler-not "movlhps" } } */
/* { dg-final { scan-assembler-not "unpckhps" } } */
/* { dg-final { scan-assembler-not "unpcklps" } } */
/* { dg-final { scan-assembler-not "shufps" } } */

typedef float __v4sf __attribute__ ((__vector_size__ (16)));
extern float bar (float, float);

__v4sf
foo1 (__v4sf x, __v4sf y)
{
  __v4sf z = { bar (x[0], y[0]), x[1], x[2], x[3] };
  return z;
}
