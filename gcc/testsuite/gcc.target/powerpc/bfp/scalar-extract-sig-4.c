/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>

unsigned __int128
get_significand (__ieee128 *p)
{
  __ieee128 source = *p;

  return __builtin_vec_scalar_extract_sig (source);	/* { dg-error "'__builtin_vsx_scalar_extract_sigq' requires" } */
}
