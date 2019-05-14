/* { dg-do compile } */
/* { dg-options "-O2 -msse -mno-sse4" } */
/* { dg-final { scan-assembler-not "movlhps" } } */
/* { dg-final { scan-assembler-not "unpckhps" } } */
/* { dg-final { scan-assembler-not "unpcklps" } } */
/* { dg-final { scan-assembler-not "shufps" } } */

typedef float __v4sf __attribute__ ((__vector_size__ (16)));
typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));

static __m128
vector_init (float f0,float f1, float f2,float f3)
{
  __v4sf y = { f0, f1, f2, f3 };
   return (__m128) y;
}

__m128
foo2 (__m128 x)
{
  return vector_init (11.4, ((__v4sf) x)[1], ((__v4sf) x)[2],
		      ((__v4sf) x) [3]);
}
