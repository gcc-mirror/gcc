/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mfpmath=sse" } */
/* { dg-final { scan-assembler-times "minss" 1 } } */
/* { dg-final { scan-assembler-not "movaps" } } */
/* { dg-final { scan-assembler-not "movss" } } */

typedef float vec __attribute__((vector_size(16)));

vec
foo (vec x, float a)
{
  x[0] = x[0] < a ? x[0] : a;
  return x;
}
