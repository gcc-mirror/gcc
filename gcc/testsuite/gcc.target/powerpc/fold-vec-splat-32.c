/* Verify that overloaded built-ins for vec_splat with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector signed int
testsi_1 ()
{
  return vec_splat_s32 (5);
}

vector signed int
testsi_2 ()
{
  return vec_splat_s32 (-5);
}

vector signed int
testsi_3 ()
{
 return vec_splat_s32 (15);
}

vector unsigned int
testui_1 ()
{
  return vec_splat_u32 (5);
}

vector unsigned int
testui_2 ()
{
  return vec_splat_u32 (-5);
}

vector unsigned int
testui_3 ()
{
  return vec_splat_u32 (15);
}

/* { dg-final { scan-assembler-times "vspltisw" 6 } } */
