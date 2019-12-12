/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9" } */

#include <stddef.h>
#include <altivec.h>

float
fetch_data (unsigned int offset, vector float *datap)
{
  vector float data = *datap;

  return vec_xlx (offset, data);
}

/* { dg-final { scan-assembler "vextuwlx" } } */
