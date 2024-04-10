/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

#include <stddef.h>
#include <altivec.h>

float
fetch_data (unsigned int offset, vector float *datap)
{
  vector float data = *datap;

  return vec_xrx (offset, data);
}

/* { dg-final { scan-assembler "vextuwrx" } } */
