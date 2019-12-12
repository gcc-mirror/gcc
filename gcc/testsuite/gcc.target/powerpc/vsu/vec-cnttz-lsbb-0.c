/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>

int
count_trailing_zero_byte_bits (vector signed char *arg1_p)
{
  vector signed char arg_1 = *arg1_p;

  return vec_cnttz_lsbb (arg_1);
}

/* { dg-final { scan-assembler "vctzlsbb" } } */
