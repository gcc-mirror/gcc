/* PR target/61925 */
/* { dg-do compile } */
/* { dg-options "-O2 -w" } */
/* { dg-additional-options "-march=i386 -mno-sse" { target ia32 } } */

#pragma GCC push_options
#pragma GCC target("sse")
typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));

void
foo (void)
{
}

__attribute__((target ("avx"))) void
bar (void)
{
}

#pragma GCC target("sse2")
#pragma GCC pop_options

__attribute__ ((vector_size (16))) int
baz (__attribute__ ((vector_size (16))) int a, __attribute__ ((vector_size (16))) int b)
{
  return a + b;
}
