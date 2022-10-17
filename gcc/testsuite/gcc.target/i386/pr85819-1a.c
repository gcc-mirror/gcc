/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx512f -mfma -mfpmath=sse" } */

float
foo (unsigned int x)
{
  return x;
}

/* { dg-final { scan-assembler "vfmadd132ss" { target ia32 } } } */
/* { dg-final { scan-assembler "vcvtsi2ssq" { target { ! ia32 } } } } */
