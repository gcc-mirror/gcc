/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -mlittle" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

int
count_leading_zero_byte_bits (vector signed char *arg1_p)
{
  vector signed char arg_1 = *arg1_p;

  return vec_cntlz_lsbb (arg_1);
}

/* { dg-final { scan-assembler "vctzlsbb" } } */
