/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2 -march=pentium4 -mtune=prescott -mfpmath=sse" } */
double foo (double x) {
  return x + 1.75;
}
/* { dg-final { scan-assembler "movsd" } } */
