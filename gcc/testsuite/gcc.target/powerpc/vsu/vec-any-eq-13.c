/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <altivec.h>

int
test_any_equal (vector bool int *arg1_p, vector bool int *arg2_p)
{
  vector bool int arg_1 = *arg1_p;
  vector bool int arg_2 = *arg2_p;

  return vec_any_eq (arg_1, arg_2);
}

/* { dg-final { scan-assembler "vcmpnew." } } */
