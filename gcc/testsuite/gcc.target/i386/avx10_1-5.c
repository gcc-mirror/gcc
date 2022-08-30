/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O0 -march=x86-64 -mavx10.1 -Wno-psabi" } */
/* { dg-final { scan-assembler-not ".%zmm" } } */

typedef double __m512d __attribute__ ((__vector_size__ (64), __may_alias__));

__m512d
foo ()
{
  __m512d a, b;
  a = a + b;
  return a;
}
