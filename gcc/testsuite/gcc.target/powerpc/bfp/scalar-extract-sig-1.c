/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target has_arch_ppc64 } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>

unsigned long long int
get_significand (double *p)
{
  double source = *p;

  return __builtin_vec_scalar_extract_sig (source);	/* { dg-error "'__builtin_vsx_scalar_extract_sig' requires" } */
}
