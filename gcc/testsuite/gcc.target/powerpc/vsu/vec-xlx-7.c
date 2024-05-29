/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <stddef.h>
#include <altivec.h>

signed int
fetch_data (unsigned int offset, vector signed int *datap)
{
  vector signed int data = *datap;

  return __builtin_vec_vextulx (offset, data);	/* { dg-error "'__builtin_altivec_vextuwlx' requires" } */
}
