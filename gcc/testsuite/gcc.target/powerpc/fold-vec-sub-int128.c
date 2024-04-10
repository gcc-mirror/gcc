/* Verify that overloaded built-ins for vec_sub with __int128
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-maltivec -mvsx" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-additional-options "-maix64" { target powerpc-ibm-aix* } } */

#include "altivec.h"

vector signed __int128
test1 (vector signed __int128 x, vector signed __int128 y)
{
  return vec_sub (x, y);
}

vector unsigned __int128
test2 (vector unsigned __int128 x, vector unsigned __int128 y)
{
  return vec_sub (x, y);
}

/* { dg-final { scan-assembler-times "vsubuqm" 2 } } */
