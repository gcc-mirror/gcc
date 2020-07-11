/* PR target/94603 */
/* { dg-do compile } */
/* { dg-options "-Wno-implicit-function-declaration -msse -mno-sse2" } */

typedef long long __attribute__ ((__vector_size__ (16))) V;

V
foo (V v)
{
  return __builtin_ia32_movq128 (v);  /* { dg-error "" } */
}
