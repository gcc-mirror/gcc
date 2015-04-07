/* { dg-do compile { target powerpc_fprs } } */
/* { dg-options "-O2 -fpic -mcpu=power5" } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power5" } } */

double foo (double x) {
  return x + 1.75;
}
/* { dg-final { scan-assembler "lfs" } } */
