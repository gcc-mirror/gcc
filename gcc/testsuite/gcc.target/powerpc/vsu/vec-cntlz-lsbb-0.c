/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */
/* { dg-additional-options "-mbig" { target powerpc64le-*-* } } */

#include <altivec.h>

int
count_leading_zero_byte_bits (vector signed char *arg1_p)
{
  vector signed char arg_1 = *arg1_p;

  return vec_cntlz_lsbb (arg_1);
}

/* { dg-final { scan-assembler "vclzlsbb" } } */
