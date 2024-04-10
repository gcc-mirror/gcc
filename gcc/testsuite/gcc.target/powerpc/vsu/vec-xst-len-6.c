/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

#include <stddef.h>
#include <altivec.h>

void
store_data (vector signed long long *datap, signed long long *address,
	    size_t length)
{
  vector signed long long data = *datap;

  vec_xst_len (data, address, length);
}

/* { dg-final { scan-assembler "sldi" } } */
/* { dg-final { scan-assembler "stxvl" } } */
