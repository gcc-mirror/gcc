/* Verify that overloaded built-ins for vec_eqv with char
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -O2" } */

#include <altivec.h>

vector bool char
test1 (vector bool char x, vector bool char y)
{
  return vec_eqv (x, y);
}

vector signed char
test3 (vector signed char x, vector signed char y)
{
  return vec_eqv (x, y);
}

vector unsigned char
test6 (vector unsigned char x, vector unsigned char y)
{
  return vec_eqv (x, y);
}

/* { dg-final { scan-assembler-times "xxleqv" 3 } } */
