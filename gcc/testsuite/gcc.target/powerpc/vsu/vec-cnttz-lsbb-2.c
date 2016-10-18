/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power8" } */

#include <altivec.h>

int
count_trailing_zero_byte_bits (vector unsigned char *arg1_p)
{
  vector unsigned char arg_1 = *arg1_p;

  return __builtin_vec_vctzlsbb (arg_1);	/* { dg-error "Builtin function __builtin_altivec_vctzlsbb requires the -mcpu=power9 option" } */
}
