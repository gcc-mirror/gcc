/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power8" } */

#include <altivec.h>

int
test_any_equal (vector unsigned int *arg1_p, vector unsigned int *arg2_p)
{
  vector unsigned int arg_1 = *arg1_p;
  vector unsigned int arg_2 = *arg2_p;

  return __builtin_vec_vcmpnez_p (__CR6_LT_REV, arg_1, arg_2);	/* { dg-error "builtin function '__builtin_vec_vcmpnez_p' not supported in this compiler configuration" } */
}
