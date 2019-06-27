/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mfpmath=sse" } */
/* { dg-final { scan-assembler-times "subsd" 1 } } */
/* { dg-final { scan-assembler-times "mulpd" 1 } } */
/* { dg-final { scan-assembler-not "movapd" } } */
/* { dg-final { scan-assembler-not "movsd" } } */

typedef double __v2df __attribute__ ((__vector_size__ (16)));

__v2df
foo (__v2df x, __v2df y)
{
  x[0] -= y[0];
  x *= y;
  return x;
}
