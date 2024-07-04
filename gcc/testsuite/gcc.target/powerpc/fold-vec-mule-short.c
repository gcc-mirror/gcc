/* Verify that overloaded built-ins for vec_mule,vec_mulo with short
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed int
test_even (vector signed short x, vector signed short y)
{
  return vec_mule (x, y);
}

vector unsigned int
test_uns_even (vector unsigned short x, vector unsigned short y)
{
  return vec_mule (x, y);
}

vector signed int
test_odd (vector signed short x, vector signed short y)
{
  return vec_mulo (x, y);
}

vector unsigned int
test_uns_odd (vector unsigned short x, vector unsigned short y)
{
  return vec_mulo (x, y);
}

/* { dg-final { scan-assembler-times "vmuleuh" 1 } } */
/* { dg-final { scan-assembler-times "vmulesh" 1 } } */
/* { dg-final { scan-assembler-times "vmulouh" 1 } } */
/* { dg-final { scan-assembler-times "vmulosh" 1 } } */
