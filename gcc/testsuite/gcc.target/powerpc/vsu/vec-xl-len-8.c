/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx } */

#include <stddef.h>
#include <altivec.h>

__vector signed short
fetch_data (signed short *address, size_t length)
{
  return vec_xl_len (address, length);
}

/* { dg-final { scan-assembler "sldi" } } */
/* { dg-final { scan-assembler "lxvl" } } */
