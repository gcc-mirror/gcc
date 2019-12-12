/* { dg-do compile { target powerpc_fprs } } */
/* { dg-options "-O2 -fpic -mdejagnu-cpu=power5" } */

double foo (double x) {
  return x + 1.75;
}
/* { dg-final { scan-assembler "lfs" } } */
