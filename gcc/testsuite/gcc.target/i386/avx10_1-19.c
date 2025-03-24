/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mno-avx10.1-512" } */
/* { dg-warning "'-mavx10.1' is aliased to 512 bit since GCC14.3 and GCC15.1 while '-mavx10.1-256' and '-mavx10.1-512' will be deprecated in GCC 16 due to all machines 512 bit vector size supported" "" { target *-*-* } 0 } */
/* { dg-final { scan-assembler "%zmm" } } */

typedef double __m512d __attribute__ ((__vector_size__ (64), __may_alias__));

__attribute__ ((target ("avx512f"))) __m512d
foo ()
{ /* { dg-warning "'-mno-avx10.1-256, -mno-avx10.1-512' cannot disable AVX512 instructions when '-mavx512XXX'" } */
  __m512d a, b;
  a = a + b;
  return a;
}
