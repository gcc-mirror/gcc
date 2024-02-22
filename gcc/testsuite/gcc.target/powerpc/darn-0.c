/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

/* This test should succeed on both 32- and 64-bit configurations.  */
#include <altivec.h>

int get_random ()
{
  return __builtin_darn_32 ();
}

/* { dg-final { scan-assembler	   "darn" } } */
