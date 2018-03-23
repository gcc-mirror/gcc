/* Verify that overloaded built-ins for vec_splat with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector signed char
testsc_1 ()
{
  return vec_splat_s8 (5);
}

vector signed char
testsc_2 ()
{
  return vec_splat_s8 (-5);
}

vector signed char
testsc_3 ()
{
  return vec_splat_s8 (15);
}

vector unsigned char
testuc_1 ()
{
  return vec_splat_u8 (5);
}

vector unsigned char
testuc_2 ()
{
  return vec_splat_u8 (-5);
}

vector unsigned char
testuc_3 ()
{
  return vec_splat_u8 (15);
}

/* { dg-final { scan-assembler-times "xxspltib|vspltisb" 6 } } */
