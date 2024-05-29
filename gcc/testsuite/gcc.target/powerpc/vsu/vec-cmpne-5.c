/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O1" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector bool int
fetch_data (vector signed int *arg1_p, vector signed int *arg2_p)
{
  vector signed int arg_1 = *arg1_p;
  vector signed int arg_2 = *arg2_p;

  return vec_cmpne (arg_1, arg_2);
}

/* { dg-final { scan-assembler "vcmpnew" } } */
