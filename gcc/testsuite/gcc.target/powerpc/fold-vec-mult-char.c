/* Verify that overloaded built-ins for vec_mul with char
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

#include <altivec.h>

vector signed char
test3 (vector signed char x, vector signed char y)
{
  return vec_mul (x, y);
}

vector unsigned char
test6 (vector unsigned char x, vector unsigned char y)
{
  return vec_mul (x, y);
}

/* { dg-final { scan-assembler-times "\[ \t\]vmulesb" 2 } } */
/* { dg-final { scan-assembler-times "\[ \t\]vmulosb" 2 } } */
