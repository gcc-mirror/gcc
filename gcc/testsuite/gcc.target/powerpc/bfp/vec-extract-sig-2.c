/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

__vector unsigned long long int
get_significands (__vector double *p)
{
  __vector double source = *p;

  return __builtin_vec_extract_sig (source);	/* { dg-error "'__builtin_vsx_extract_sig_dp' requires" } */
}
