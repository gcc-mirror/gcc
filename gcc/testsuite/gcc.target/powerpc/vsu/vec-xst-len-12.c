/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx } */

#include <stddef.h>
#include <altivec.h>

/* The vec_xst_len() function is not available on power8 configurations.  */

void
store_data (vector double *datap, double *address, size_t length)
{
  vector double data = *datap;

  __builtin_vec_stxvl (data, address, length); /* { dg-error "'__builtin_altivec_stxvl' requires the" } */
}
