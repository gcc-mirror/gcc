/* Verify that overloaded built-ins for vec_splat with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector signed short
testss_1 ()
{
  return vec_splat_s16 (5);
}

vector signed short
testss_2 ()
{
  return vec_splat_s16 (-5);
}

vector signed short
testss_3 ()
{
  return vec_splat_s16 (15);
}

vector unsigned short
testus_1 ()
{
  return vec_splat_u16 (5);
}

vector unsigned short
testus_2 ()
{
  return vec_splat_u16 (-5);
}

vector unsigned short
testus_3 ()
{
  return vec_splat_u16 (15);
}

/* { dg-final { scan-assembler-times "vspltish" 6 } } */
