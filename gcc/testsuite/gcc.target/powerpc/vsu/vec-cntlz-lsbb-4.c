/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -mlittle" } */

#include <altivec.h>

int
count_leading_zero_byte_bits (vector unsigned char *arg1_p)
{
  vector unsigned char arg_1 = *arg1_p;

  return vec_cntlz_lsbb (arg_1);
}

/* { dg-final { scan-assembler "vctzlsbb" } } */
