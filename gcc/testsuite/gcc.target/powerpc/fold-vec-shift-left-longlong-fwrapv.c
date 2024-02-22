/* Verify that overloaded built-ins for vec_sl produce the right results.  */
/* This test covers the shift left tests with the -fwrapv option. */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2 -fwrapv" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

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
