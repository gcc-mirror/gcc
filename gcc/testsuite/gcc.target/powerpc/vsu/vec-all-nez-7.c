/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power8" } */

#include <altivec.h>

int
test_all_not_equal_and_not_zero (vector unsigned short *arg1_p,
				 vector unsigned short *arg2_p)
{
  vector unsigned short arg_1 = *arg1_p;
  vector unsigned short arg_2 = *arg2_p;

  return __builtin_vec_vcmpnez_p (__CR6_LT, arg_1, arg_2);	/* { dg-error "Builtin function __builtin_altivec_vcmpnezh_p requires" } */
}
