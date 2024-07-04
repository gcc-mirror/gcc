/* Verify that overloaded built-ins for vec_merge* with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2 " } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector bool int
testbi_l (vector bool int vbi2, vector bool int vbi3)
{
  return vec_mergel (vbi2, vbi3);
}

vector signed int
testsi_l (vector signed int vsi2, vector signed int vsi3)
{
  return vec_mergel (vsi2, vsi3);
}

vector unsigned int
testui_l (vector unsigned int vui2, vector unsigned int vui3)
{
  return vec_mergel (vui2, vui3);
}

vector bool int
testbi_h (vector bool int vbi2, vector bool int vbi3)
{
  return vec_mergeh (vbi2, vbi3);
}

vector signed int
testsi_h (vector signed int vsi2, vector signed int vsi3)
{
  return vec_mergeh (vsi2, vsi3);
}

vector unsigned int
testui_h (vector unsigned int vui2, vector unsigned int vui3)
{
  return vec_mergeh (vui2, vui3);
}

/* { dg-final { scan-assembler-times "vmrghw|xxmrghw" 3 } } */
/* { dg-final { scan-assembler-times "vmrglw|xxmrglw" 3 } } */

