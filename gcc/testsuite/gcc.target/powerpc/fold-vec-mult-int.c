/* Verify that overloaded built-ins for vec_mul with int
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-maltivec -mvsx -mpower8-vector" } */

#include <altivec.h>

vector signed int
test3 (vector signed int x, vector signed int y)
{
  return vec_mul (x, y);
}

vector unsigned int
test6 (vector unsigned int x, vector unsigned int y)
{
  return vec_mul (x, y);
}

/* { dg-final { scan-assembler-times "\[ \t\]vmuluwm" 2 } } */

