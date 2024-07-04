/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

int
test_all_not_equal (vector bool long long *arg1_p, vector bool long long *arg2_p)
{
  vector bool long long arg_1 = *arg1_p;
  vector bool long long arg_2 = *arg2_p;

  return vec_all_ne (arg_1, arg_2);
}

/* { dg-final { scan-assembler "vcmpequd." } } */
