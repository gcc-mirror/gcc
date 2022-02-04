/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mlittle" } */

#include <altivec.h>

int
count_trailing_zero_byte_bits (vector unsigned char *arg1_p)
{
  vector unsigned char arg_1 = *arg1_p;

  return vec_cnttz_lsbb (arg_1);
}

/* { dg-final { scan-assembler "vclzlsbb" } } */
