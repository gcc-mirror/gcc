/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mavx512f" } */
/* { dg-final { scan-assembler "%zmm" } } */

typedef double __m512d __attribute__ ((__vector_size__ (64), __may_alias__));

__attribute__ ((target ("avx10.1"))) __m512d
foo ()
{ /* { dg-warning "Vector size conflicts between AVX10.1 and AVX512, using 512 as max vector size" } */
  __m512d a, b;
  a = a + b;
  return a;
}
