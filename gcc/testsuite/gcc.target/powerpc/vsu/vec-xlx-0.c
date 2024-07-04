/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <stddef.h>
#include <altivec.h>

signed char
fetch_data (unsigned int offset, vector signed char *datap)
{
  vector signed char data = *datap;

  return vec_xlx (offset, data);
}

/* { dg-final { scan-assembler "vextublx" } } */
