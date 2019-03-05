/* Verify that overloaded built-ins for vec_sel with char
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-maltivec -O2" } */

#include <altivec.h>

vector bool char
test1_0 (vector bool char x, vector bool char y, vector bool char z)
{
  return vec_sel (x, y, z);
}

vector bool char
test1_1 (vector bool char x,vector bool char y, vector unsigned char z)
{
  return vec_sel (x, y, z);
}

vector signed char
test3_0 (vector signed char x,vector signed char y, vector bool char  z)
{
  return vec_sel (x, y, z);
}

vector signed char
test3_1 (vector signed char x,vector signed char  y, vector unsigned char z)
{
  return vec_sel (x, y, z);
}

vector unsigned char
test6_0 (vector unsigned char x,vector unsigned char  y,vector bool char  z)
{
  return vec_sel (x, y, z);
}

vector unsigned char
test6_1 (vector unsigned char x,vector unsigned char  y, vector unsigned char z)
{
  return vec_sel (x, y, z);
}

/* { dg-final { scan-assembler-times {\mxxsel\M|\mvsel\M} 6 } } */
