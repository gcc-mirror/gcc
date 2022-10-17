/* { dg-do compile } */
/* { dg-options "-O2 -msse -mfpmath=sse" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */
/* { dg-final { scan-assembler-times "divss" 1 } } */
/* { dg-final { scan-assembler-not "movaps" } } */
/* { dg-final { scan-assembler-not "movss" } } */

typedef float vec __attribute__((vector_size(16)));

vec
foo (vec x)
{
  x[0] /= 2.1f;
  return x;
}
