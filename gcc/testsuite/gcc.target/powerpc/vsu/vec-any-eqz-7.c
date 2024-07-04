/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

int
test_any_equal (vector unsigned int *arg1_p, vector unsigned int *arg2_p)
{
  vector unsigned int arg_1 = *arg1_p;
  vector unsigned int arg_2 = *arg2_p;

  return __builtin_vec_vcmpnez_p (__CR6_LT_REV, arg_1, arg_2);
  /* { dg-error "'__builtin_altivec_vcmpnezw_p' requires the '-mcpu=power9' and '-mvsx' options" "" { target *-*-* } .-1 } */
}
