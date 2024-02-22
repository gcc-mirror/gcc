/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

#include <altivec.h>

long get_raw_random ()
{
  return __builtin_darn_raw ();
}

/* { dg-final { scan-assembler	   "darn" } } */
