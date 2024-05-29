/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */

/* This test should succeed on both 32- and 64-bit configurations.  */
#include <altivec.h>

int get_random ()
{
  return __builtin_darn_32 ();
}

/* { dg-final { scan-assembler	   "darn" } } */
