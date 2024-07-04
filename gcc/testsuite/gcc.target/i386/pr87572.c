/* PR target/82483 */
/* { dg-do compile } */
/* { dg-options "-fpermissive -O2 -mavx512ifma -mno-sse2 -w -Wno-psabi" } */

typedef long long __m512i __attribute__((__vector_size__(64)));
__m512i
foo (__m512i c, __m512i d, __m512i e, int b)
{
  return __builtin_ia32_vpmadd52huq512_maskz (c, d, e, b); /* { dg-error "incompatible types" } */
}
