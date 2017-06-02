/* Verify that overloaded built-ins for vec_min with char
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

#include <altivec.h>

vector signed char
test3_min (vector signed char x, vector signed char y)
{
  return vec_min (x, y);
}

vector unsigned char
test6_min (vector unsigned char x, vector unsigned char y)
{
  return vec_min (x, y);
}

vector signed char
test3_max (vector signed char x, vector signed char y)
{
  return vec_max (x, y);
}

vector unsigned char
test6_max (vector unsigned char x, vector unsigned char y)
{
  return vec_max (x, y);
}

/* { dg-final { scan-assembler-times "vminsb" 1 } } */
/* { dg-final { scan-assembler-times "vmaxsb" 1 } } */
/* { dg-final { scan-assembler-times "vminub" 1 } } */
/* { dg-final { scan-assembler-times "vmaxub" 1 } } */
