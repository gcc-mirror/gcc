/* Verify that overloaded built-ins for vec_sl produce the right results.  */
/* This test covers the shift left tests with the -fwrapv option. */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2 -mpower8-vector -fwrapv" } */

#include <altivec.h>

vector signed long long
testsl_signed_longlong (vector signed long long x, vector unsigned long long y)
{
  return vec_sl (x, y);
}

vector unsigned long long
testsl_unsigned_longlong (vector unsigned long long x, vector unsigned long long y)
{
  return vec_sl (x, y);
}

/* { dg-final { scan-assembler-times "vsld" 2 } } */
