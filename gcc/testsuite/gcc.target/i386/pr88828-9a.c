/* { dg-do compile } */
/* { dg-options "-O2 -msse -mno-sse4" } */
/* { dg-final { scan-assembler-not "movlhps" } } */
/* { dg-final { scan-assembler-not "unpckhps" } } */
/* { dg-final { scan-assembler-not "unpcklps" } } */
/* { dg-final { scan-assembler-not "shufps" } } */

typedef float __v4sf __attribute__ ((__vector_size__ (16)));
typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));

__m128
foo1 (__m128 x)
{
  __v4sf z = { 11.4, ((__v4sf) x)[1], ((__v4sf) x)[2], ((__v4sf) x) [3] };
  return (__m128) z;
}
