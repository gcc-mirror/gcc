/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O1" } */

#include <altivec.h>

vector bool int
fetch_data (vector unsigned int *arg1_p, vector unsigned int *arg2_p)
{
  vector unsigned int arg_1 = *arg1_p;
  vector unsigned int arg_2 = *arg2_p;

  return vec_cmpne (arg_1, arg_2);
}

/* { dg-final { scan-assembler "vcmpnew" } } */
