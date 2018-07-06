/* Verify that overloaded built-ins for vec_splat_s8 and vec_splat_s16
   generate errors as expected when we attempt to use invalid inputs.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */

#include <altivec.h>

vector signed short
testss_1 (unsigned int ui)
{
  return vec_splat_s16 (ui);/* { dg-error "argument 1 must be a 5-bit signed literal" } */
}

vector unsigned short
testss_2 (signed int si)
{
  return vec_splat_u16 (si);/* { dg-error "argument 1 must be a 5-bit signed literal" } */
}

vector signed char
testsc_1 (unsigned int ui)
{
  return vec_splat_s8 (ui); /* { dg-error "argument 1 must be a 5-bit signed literal" } */
}

vector unsigned char
testsc_2 (signed int si)
{
  return vec_splat_u8 (si);/* { dg-error "argument 1 must be a 5-bit signed literal" } */
}

