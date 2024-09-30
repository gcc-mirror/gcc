/*
 Test the least significant bit by byte instruction
    xvtlsbb BF,XB
 Using the builtins
    int vec_test_lsbb_all_zeros (vector unsigned char);
    int vec_test_lsbb_all_ones (vector unsigned char);
 */

/* { dg-require-effective-target } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* { dg-final { scan-assembler-times {\mxvtlsbb\M} 6 } } */
/* { dg-final { scan-assembler-times {\msetbc\M} 6 } } */

#include <altivec.h>

int test_for_zeros_signed(vector char vc) {
  return vec_test_lsbb_all_zeros(vc);
}

int test_for_zeros_unsigned(vector unsigned char vuc) {
  return vec_test_lsbb_all_zeros(vuc);
}

int test_for_zeros_bool(vector bool char vbc) {
  return vec_test_lsbb_all_zeros(vbc);
}

int test_for_ones_signed(vector signed char vc) {
  return vec_test_lsbb_all_ones(vc);
}

int test_for_ones_unsigned(vector unsigned char vuc) {
  return vec_test_lsbb_all_ones(vuc);
}

int test_for_ones_bool(vector bool char vbc) {
  return vec_test_lsbb_all_ones(vbc);
}

