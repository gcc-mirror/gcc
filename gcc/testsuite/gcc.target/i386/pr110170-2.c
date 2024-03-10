/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-msse2 -O2 -mfpmath=sse" } */
/* { dg-final { scan-assembler-not "comi" } }  */

double
foo (double* a, double* b, double c, double d)
{
  return *a < *b ? c : d;
}

float
foo1 (float* a, float* b, float c, float d)
{
  return *a < *b ? c : d;
}

