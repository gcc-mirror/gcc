/* Verify that overloaded built-ins for vec_mul with __int128
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_float128_hw_ok } */
/* { dg-require-effective-target int128 } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-maltivec -mvsx -mcpu=power9 -O2" } */
/* { dg-additional-options "-maix64" { target powerpc-ibm-aix* } } */

#include "altivec.h"

vector signed __int128
test1 (vector signed __int128 x, vector signed __int128 y)
{
  return vec_mul (x, y);
}

vector unsigned __int128
test2 (vector unsigned __int128 x, vector unsigned __int128 y)
{
  return vec_mul (x, y);
}

/* { dg-final { scan-assembler-times "\[ \t\]xsmulqp" 2 } } */
