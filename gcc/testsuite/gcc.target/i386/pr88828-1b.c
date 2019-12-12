/* { dg-do compile } */
/* { dg-options "-O2 -msse -mno-sse4" } */
/* { dg-final { scan-assembler "movss" } } */
/* { dg-final { scan-assembler-not "movaps" } } */
/* { dg-final { scan-assembler-not "movlhps" } } */
/* { dg-final { scan-assembler-not "unpcklps" } } */
/* { dg-final { scan-assembler-not "shufps" } } */

typedef float __v4sf __attribute__ ((__vector_size__ (16)));

static __v4sf
vector_init (float f0,float f1, float f2,float f3)
{
  __v4sf y = { f0, f1, f2, f3 };
   return y;
}

__attribute__((noinline, noclone))
__v4sf
foo2 (__v4sf x, float f)
{
  return vector_init (f, x[1], x[2], x[3]) ;
}
