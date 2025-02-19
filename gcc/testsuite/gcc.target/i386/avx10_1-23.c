/* { dg-do compile } */
/* { dg-options "-march=x86-64 -mevex512 -Wno-psabi" } */
/* { dg-final { scan-assembler-not "%zmm" } } */

typedef double __m512d __attribute__ ((__vector_size__ (64), __may_alias__));

__attribute__ ((target ("avx10.1-256"))) __m512d
foo ()
{ /* { dg-warning "Using '-mevex512' without any AVX512 features enabled together with AVX10.1 only will not enable any AVX512 or AVX10.1-512 features, using 256 as max vector size" "" { target *-*-* } 0 } */
  __m512d a, b;
  a = a + b;
  return a;
}
