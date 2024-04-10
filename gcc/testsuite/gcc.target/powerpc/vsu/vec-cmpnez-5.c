/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

#include <altivec.h>

vector bool int
fetch_data (vector signed int *arg1_p, vector signed int *arg2_p)
{
  vector signed int arg_1 = *arg1_p;
  vector signed int arg_2 = *arg2_p;

  return vec_cmpnez (arg_1, arg_2);
}

/* { dg-final { scan-assembler "vcmpnezw" } } */
