/* Verify that overloaded built-ins for vec_mule,vec_mulo with char
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed short
test_even (vector signed char x, vector signed char y)
{
  return vec_mule (x, y);
}

vector unsigned short
test_uns_even (vector unsigned char x, vector unsigned char y)
{
  return vec_mule (x, y);
}

vector signed short
test_odd (vector signed char x, vector signed char y)
{
  return vec_mulo (x, y);
}

vector unsigned short
test_uns_odd (vector unsigned char x, vector unsigned char y)
{
  return vec_mulo (x, y);
}

/* { dg-final { scan-assembler-times "vmuleub" 1 } } */
/* { dg-final { scan-assembler-times "vmulesb" 1 } } */
/* { dg-final { scan-assembler-times "vmuloub" 1 } } */
/* { dg-final { scan-assembler-times "vmulosb" 1 } } */

