/* Verify that overloaded built-ins for vec_merge* with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector bool short
testbi_el (vector bool short vbs2, vector bool short vbs3)
{
  return vec_mergel (vbs2, vbs3);
}

vector signed short
testsi_el (vector signed short vss2, vector signed short vss3)
{
  return vec_mergel (vss2, vss3);
}

vector unsigned short
testui_el (vector unsigned short vus2, vector unsigned short vus3)
{
  return vec_mergel (vus2, vus3);
}

vector bool short
testbi_eh (vector bool short vbs2, vector bool short vbs3)
{
  return vec_mergeh (vbs2, vbs3);
}

vector signed short
testsi_eh (vector signed short vss2, vector signed short vss3)
{
  return vec_mergeh (vss2, vss3);
}

vector unsigned short
testui_eh (vector unsigned short vus2, vector unsigned short vus3)
{
  return vec_mergeh (vus2, vus3);
}

/* { dg-final { scan-assembler-times "vmrghh" 3 } } */
/* { dg-final { scan-assembler-times "vmrglh" 3 } } */

