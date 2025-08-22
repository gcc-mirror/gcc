/* PR target/121634: ICE in highway-1.3.0 testsuite */
/* { dg-do compile } */
/* { dg-options "-O2 -march=la464 -mabi=lp64d" } */

typedef short v8i16 __attribute__ ((vector_size (16)));
typedef int v4i32 __attribute__ ((vector_size (16)));
typedef long __m128i __attribute__ ((__vector_size__ (16)));
__m128i x, y;

__m128i
WidenMulPairwiseAdd (__m128i a, __m128i b)
{
  y = (__m128i)__builtin_lsx_vmaddwod_w_h ((v4i32)x, (v8i16){}, (v8i16){});
  return y;
}
