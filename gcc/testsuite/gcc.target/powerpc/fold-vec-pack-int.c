/* Verify that overloaded built-ins for vec_pack with int
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector bool short
testbi_h (vector bool int vbi2, vector bool int vbi3)
{
  return vec_pack (vbi2, vbi3);
}

vector signed short
testsi_h (vector signed int vsi2, vector signed int vsi3)
{
  return vec_pack (vsi2, vsi3);
}

vector unsigned short
testui_h (vector unsigned int vui2, vector unsigned int vui3)
{
  return vec_pack (vui2, vui3);
}

/* { dg-final { scan-assembler-times "vpkuwum" 3 } } */
