/* Verify that overloaded built-ins for vec_pack with short
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector bool char
testbi_eh (vector bool short vbs2, vector bool short vbs3)
{
  return vec_pack (vbs2, vbs3);
}

vector signed char
testsi_eh (vector signed short vss2, vector signed short vss3)
{
  return vec_pack (vss2, vss3);
}

vector unsigned char
testui_eh (vector unsigned short vus2, vector unsigned short vus3)
{
  return vec_pack (vus2, vus3);
}

/* { dg-final { scan-assembler-times "vpkuhum" 3 } } */
