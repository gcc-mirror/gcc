/* { dg-do compile { target powerpc_fprs } } */
/* { dg-options "-O2" } */
double foo (double x) {
  return x + 1.75;
}
/* { dg-final { scan-assembler "lfs" } } */
