/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mfpmath=sse" } */
/* { dg-final { scan-assembler-times "maxsd" 1 } } */
/* { dg-final { scan-assembler-not "movapd" } } */
/* { dg-final { scan-assembler-not "movsd" } } */

typedef double vec __attribute__((vector_size(16)));

vec
foo (vec x, double a)
{
  x[0] = x[0] > a ? x[0] : a;
  return x;
}
