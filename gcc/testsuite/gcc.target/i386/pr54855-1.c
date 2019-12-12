/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mfpmath=sse" } */
/* { dg-final { scan-assembler-times "addsd" 1 } } */
/* { dg-final { scan-assembler-not "movapd" } } */
/* { dg-final { scan-assembler-not "movsd" } } */

typedef double __v2df __attribute__ ((__vector_size__ (16)));
typedef double __m128d __attribute__ ((__vector_size__ (16), __may_alias__));

__m128d
_mm_add_sd (__m128d x, __m128d y)
{
  __m128d z =  __extension__ (__m128d)(__v2df)
    { (((__v2df) x)[0] + ((__v2df) y)[0]), ((__v2df) x)[1] };
  return z;
}
