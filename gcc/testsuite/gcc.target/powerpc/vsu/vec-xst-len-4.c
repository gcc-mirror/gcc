/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx } */

#include <stddef.h>
#include <altivec.h>

void
store_data (vector signed __int128 *datap, signed __int128 *address,
	    size_t length)
{
  vector signed __int128 data = *datap;

  vec_xst_len (data, address, length);
}

/* { dg-final { scan-assembler "sldi" } } */
/* { dg-final { scan-assembler "stxvl" } } */
