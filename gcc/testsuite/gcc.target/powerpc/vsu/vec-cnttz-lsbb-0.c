/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */
/* { dg-additional-options "-mbig" { target powerpc64le-*-* } } */

#include <altivec.h>

int
count_trailing_zero_byte_bits (vector signed char *arg1_p)
{
  vector signed char arg_1 = *arg1_p;

  return vec_cnttz_lsbb (arg_1);
}

/* { dg-final { scan-assembler "vctzlsbb" } } */
