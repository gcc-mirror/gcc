/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mfpmath=sse" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */
/* { dg-final { scan-assembler-times "subsd" 1 } } */
/* { dg-final { scan-assembler-not "movapd" } } */
/* { dg-final { scan-assembler-not "movsd" } } */

typedef double vec __attribute__((vector_size(16)));

vec
foo (vec x)
{
  x[0] -= 1.;
  return x;
}
