/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8" } */

#include <stddef.h>
#include <altivec.h>

signed short
fetch_data (unsigned short offset, vector signed short *datap)
{
  vector signed short data = *datap;

  return __builtin_vec_vexturx (offset, data);	/* { dg-error "builtin function '__builtin_altivec_vextuhrx' requires" } */
}
