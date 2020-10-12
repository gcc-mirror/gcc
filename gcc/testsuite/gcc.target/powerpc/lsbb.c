/*
 Test the least significant bit by byte instruction
    xvtlsbb BF,XB
 Using the builtins
    int vec_test_lsbb_all_zeros (vector unsigned char);
    int vec_test_lsbb_all_ones (vector unsigned char);
 */

/* { dg-require-effective-target power10_ok } */
/* { dg-options "-fno-inline -mdejagnu-cpu=power10 -O2" } */

/* { dg-final { scan-assembler-times {\mxvtlsbb\M} 2 } } */
/* { dg-final { scan-assembler-times {\msetbc\M} 2 } } */

#include <altivec.h>

int test_for_zeros(vector char vc) {
  return vec_test_lsbb_all_zeros(vc);
}

int test_for_ones(vector char vc) {
  return vec_test_lsbb_all_ones(vc);
}

