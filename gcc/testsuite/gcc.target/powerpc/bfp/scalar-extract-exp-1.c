/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8" } */

#include <altivec.h>

unsigned int
get_exponent (double *p)
{
  double source = *p;

  return __builtin_vec_scalar_extract_exp (source); /* { dg-error "'__builtin_vsx_scalar_extract_exp' requires" } */
}


