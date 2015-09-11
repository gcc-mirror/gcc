/* PR target/61925 */
/* { dg-do compile } */
/* { dg-options "-O2 -w" } */
/* { dg-additional-options "-march=i386 -mno-sse" { target ia32 } } */

#pragma GCC push_options
#pragma GCC target("sse")
typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));
typedef long long __m128i __attribute__ ((__vector_size__ (16), __may_alias__));
__m128i
bar (__m128 __A)
{
}

#pragma GCC pop_options

__attribute__ ((vector_size (16))) int
foo (__attribute__ ((vector_size (16))) int a, __attribute__ ((vector_size (16))) int b)
{
  return a + b;
}
