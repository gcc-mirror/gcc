/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mno-avx10.1-512" } */
/* { dg-final { scan-assembler "%zmm" } } */

typedef double __m512d __attribute__ ((__vector_size__ (64), __may_alias__));

__attribute__ ((target ("avx512f"))) __m512d
foo ()
{ /* { dg-warning "'-mno-avx10.1-256, -mno-avx10.1-512' cannot disable AVX512 instructions when '-mavx512XXX'" } */
  __m512d a, b;
  a = a + b;
  return a;
}
