/* { dg-do compile } */
/* { dg-options "-O0 -march=x86-64 -mavx512f -mno-evex512 -Wno-psabi" } */
/* { dg-warning "'-mevex512' will be deprecated in GCC 16 due to all machines 512 bit vector size supported" "" { target *-*-* } 0 } */
/* { dg-final { scan-assembler-not ".%zmm" } } */

typedef double __m512d __attribute__ ((__vector_size__ (64), __may_alias__));

__m512d
foo ()
{
  __m512d a, b;
  a = a + b;
  return a;
}
