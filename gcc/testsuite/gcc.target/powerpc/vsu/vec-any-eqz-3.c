/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>

int
test_any_equal_or_zero (vector signed short *arg1_p,
			vector signed short *arg2_p)
{
  vector signed short arg_1 = *arg1_p;
  vector signed short arg_2 = *arg2_p;

  return vec_any_eqz (arg_1, arg_2);
}

/* { dg-final { scan-assembler "vcmpnezh." } } */
