/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

__vector unsigned int
get_significands (__vector float *p)
{
  __vector float source = *p;

  return __builtin_vec_extract_sig (source);	/* { dg-error "'__builtin_vsx_extract_sig_sp' requires" } */
}
