/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

int
test_any_equal (vector double *arg1_p, vector double *arg2_p)
{
  vector double arg_1 = *arg1_p;
  vector double arg_2 = *arg2_p;

  return vec_any_eq (arg_1, arg_2);
}

/* { dg-final { scan-assembler "xvcmpeqdp." } } */
/* { dg-final { scan-assembler "rlwinm r?\[0-9\]+,r?\[0-9\]+,27,1" } } */
