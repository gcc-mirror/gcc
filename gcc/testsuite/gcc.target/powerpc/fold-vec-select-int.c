/* Verify that overloaded built-ins for vec_sel with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector bool int
test1_0 (vector bool int x, vector bool int y, vector bool int z)
{
  return vec_sel (x, y, z);
}

vector bool int
test1_1 (vector bool int x, vector bool int y, vector unsigned int z)
{
  return vec_sel (x, y, z);
}

vector signed int
test3_0 (vector signed int x, vector signed int y, vector bool int z)
{
  return vec_sel (x, y, z);
}

vector signed int
test3_1 (vector signed int x, vector signed int y, vector unsigned int z)
{
  return vec_sel (x, y, z);
}

vector unsigned int
test6_0 (vector unsigned int x, vector unsigned int y, vector bool int z)
{
  return vec_sel (x, y, z);
}

vector unsigned int
test6_1 (vector unsigned int x, vector unsigned int y, vector unsigned int z)
{
  return vec_sel (x, y, z);
}

/* { dg-final { scan-assembler-times {\mxxsel\M|\mvsel\M} 6 } } */
