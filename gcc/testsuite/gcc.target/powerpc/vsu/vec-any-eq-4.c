/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

int
test_any_equal (vector unsigned short *arg1_p,
		vector unsigned short *arg2_p)
{
  vector unsigned short arg_1 = *arg1_p;
  vector unsigned short arg_2 = *arg2_p;

  return vec_any_eq (arg_1, arg_2);
}

/* { dg-final { scan-assembler "vcmpneh." } } */
