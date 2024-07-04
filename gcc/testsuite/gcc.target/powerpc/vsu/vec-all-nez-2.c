/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

int
test_all_not_equal_and_not_zero (vector unsigned char *arg1_p,
				 vector unsigned char *arg2_p)
{
  vector unsigned char arg_1 = *arg1_p;
  vector unsigned char arg_2 = *arg2_p;

  return vec_all_nez (arg_1, arg_2);
}

/* { dg-final { scan-assembler "vcmpnezb." } } */
