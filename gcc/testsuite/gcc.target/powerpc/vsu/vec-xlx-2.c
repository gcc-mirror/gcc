/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <stddef.h>
#include <altivec.h>

signed short
fetch_data (unsigned int offset, vector signed short *datap)
{
  vector signed short data = *datap;

  return vec_xlx (offset, data);
}

/* { dg-final { scan-assembler "vextuhlx" } } */
