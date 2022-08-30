/* { dg-do compile } */
/* { dg-options "-march=x86-64" } */
/* { dg-final { scan-assembler "%zmm" } } */

typedef double __m512d __attribute__ ((__vector_size__ (64), __may_alias__));

__attribute__ ((target ("avx10.1-512"))) __m512d
foo ()
{
  __m512d a, b;
  a = a + b;
  return a;
}
