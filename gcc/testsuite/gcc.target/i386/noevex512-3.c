/* { dg-do compile } */
/* { dg-options "-march=x86-64 -Wno-psabi -mavx512f" } */
/* { dg-final { scan-assembler-not ".%zmm" } } */

typedef double __m512d __attribute__ ((__vector_size__ (64), __may_alias__));

__attribute__ ((target ("no-evex512"))) __m512d
foo ()
{
  __m512d a, b;
  a = a + b;
  return a;
}
