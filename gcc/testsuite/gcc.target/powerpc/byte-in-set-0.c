/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx" } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>

int
test_byte_in_set (unsigned char b, unsigned long long set_members)
{
  return __builtin_byte_in_set (b, set_members);
}

/* { dg-final { scan-assembler "cmpeqb" } } */
/* { dg-final { scan-assembler "setb" } } */
