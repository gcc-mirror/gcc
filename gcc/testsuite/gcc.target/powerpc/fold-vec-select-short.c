/* Verify that overloaded built-ins for vec_sel with short
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector bool short
test1_0 (vector bool short x, vector bool short y, vector bool short z)
{
  return vec_sel (x, y, z);
}

vector bool short
test1_1 (vector bool short x, vector bool short y, vector unsigned short z)
{
  return vec_sel (x, y, z);
}

vector signed short
test3_0 (vector signed short x, vector signed short y, vector bool short z)
{
  return vec_sel (x, y, z);
}

vector signed short
test3_1 (vector signed short x, vector signed short y, vector unsigned short z)
{
  return vec_sel (x, y, z);
}

vector unsigned short
test6_0 (vector unsigned short x, vector unsigned short y, vector bool short z)
{
  return vec_sel (x, y, z);
}

vector unsigned short
test6_1 (vector unsigned short x, vector unsigned short y, vector unsigned short z)
{
  return vec_sel (x, y, z);
}

/* { dg-final { scan-assembler-times {\mxxsel\M|\mvsel\M} 6 } } */
