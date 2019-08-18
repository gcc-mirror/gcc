/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8" } */

#include <stddef.h>
#include <altivec.h>

signed int
fetch_data (unsigned int offset, vector signed int *datap)
{
  vector signed int data = *datap;

  return __builtin_vec_vextulx (offset, data);	/* { dg-error "'__builtin_altivec_vextuwlx' requires" } */
}
