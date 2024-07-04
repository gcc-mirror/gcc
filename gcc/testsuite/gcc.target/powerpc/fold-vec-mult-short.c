/* Verify that overloaded built-ins for vec_mul with short
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed short
test3 (vector signed short x, vector signed short y)
{
  return vec_mul (x, y);
}

vector unsigned short
test6 (vector unsigned short x, vector unsigned short y)
{
  return vec_mul (x, y);
}

/* { dg-final { scan-assembler-times "\[ \t\]vmladduhm" 2 } } */

