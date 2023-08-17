/* { dg-do compile } */
/* { dg-options "-march=x86-64" } */
/* { dg-final { scan-assembler "%zmm" } } */

typedef double __m512d __attribute__ ((__vector_size__ (64), __may_alias__));

__attribute__ ((target ("avx10.1-256,avx10.1-512"))) __m512d
foo ()
{ /* { dg-warning "The options used for AVX10 have conflict vector width, using the latter 512 as vector width" } */
  __m512d a, b;
  a = a + b;
  return a;
}
