/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mavx512f -mno-evex512" } */
/* { dg-final { scan-assembler "%zmm" } } */

typedef double __m512d __attribute__ ((__vector_size__ (64), __may_alias__));

__attribute__ ((target ("avx10.1-512"))) __m512d
foo ()
{ /* { dg-warning "'-mno-evex512' or '-mno-avx512XXX' cannot disable AVX10 instructions when AVX10.1-512 is available" } */
  __m512d a, b;
  a = a + b;
  return a;
}
