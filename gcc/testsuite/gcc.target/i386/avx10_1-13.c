/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mavx10.1-256" } */
/* { dg-warning "'-mavx10.1' is aliased to 512 bit since GCC14.3 and GCC15.1 while '-mavx10.1-256' and '-mavx10.1-512' will be deprecated in GCC 16 due to all machines 512 bit vector size supported" "" { target *-*-* } 0 } */
/* { dg-final { scan-assembler "%zmm" } } */

typedef double __m512d __attribute__ ((__vector_size__ (64), __may_alias__));

__attribute__ ((target ("avx512f"))) __m512d
foo ()
{ /* { dg-warning "Vector size conflicts between AVX10.1 and AVX512, using 512 as max vector size" } */
  __m512d a, b;
  a = a + b;
  return a;
}
