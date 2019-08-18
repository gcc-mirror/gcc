/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8" } */

#include <altivec.h>

int
count_leading_zero_byte_bits (vector unsigned char *arg1_p)
{
  vector unsigned char arg_1 = *arg1_p;

  return __builtin_vec_vclzlsbb (arg_1);	/* { dg-error "'__builtin_altivec_vclzlsbb_v16qi' requires the '-mcpu=power9' option" } */
}
