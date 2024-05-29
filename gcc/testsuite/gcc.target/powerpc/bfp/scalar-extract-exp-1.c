/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

unsigned int
get_exponent (double *p)
{
  double source = *p;

  return __builtin_vec_scalar_extract_exp (source); /* { dg-error "'__builtin_vsx_scalar_extract_exp' requires" } */
}


