/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_vsx } */

/* This test only runs on 32-bit configurations, where a compiler error
   should be issued because this builtin is not available on
   32-bit configurations.  */

#include <altivec.h>

__ieee128
insert_exponent (unsigned __int128 *significand_p, /* { dg-error "'__int128' is not supported on this target" } */
		 unsigned long long int *exponent_p)
{
  unsigned __int128 significand = *significand_p;  /* { dg-error "'__int128' is not supported on this target" } */
  unsigned long long int exponent = *exponent_p;

  return scalar_insert_exp (significand, exponent); /* { dg-error "'__builtin_vsx_scalar_insert_exp' requires the" } */
}
