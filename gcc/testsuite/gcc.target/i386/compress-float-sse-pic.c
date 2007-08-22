/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -march=pentium4 -mtune=prescott -mfpmath=sse -fpic" } */
double foo (double x) {
  return x + 1.75;
}
/* { dg-final { scan-assembler "movsd" } } */
