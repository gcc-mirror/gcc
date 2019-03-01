/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>

int
test_all_not_equal_and_not_zero (vector unsigned short *arg1_p,
				 vector unsigned short *arg2_p)
{
  vector unsigned short arg_1 = *arg1_p;
  vector unsigned short arg_2 = *arg2_p;

  return vec_all_nez (arg_1, arg_2);
}

/* { dg-final { scan-assembler "vcmpnezh." } } */
