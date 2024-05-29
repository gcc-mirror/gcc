/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { has_arch_ppc64 } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

/* This test only runs on 32-bit configurations, producing a compiler
   error because the builtin requires 64 bits.  */
#include <altivec.h>

unsigned long long int
get_significand (double *p)
{
  double source = *p;

  return __builtin_vec_scalar_extract_sig (source); /* { dg-error "'__builtin_vsx_scalar_extract_sig' requires the" } */
}
