/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

__vector unsigned long long int
get_exponents (__vector double *p)
{
  __vector double source = *p;

  return __builtin_vec_extract_exp (source); /* { dg-error "'__builtin_vsx_extract_exp_dp' requires" } */
}
