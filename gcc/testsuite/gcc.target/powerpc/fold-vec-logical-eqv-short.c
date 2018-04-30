/* Verify that overloaded built-ins for vec_eqv with short
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -O2" } */

#include <altivec.h>

vector bool short
test1 (vector bool short x, vector bool short y)
{
  return vec_eqv (x, y);
}

vector signed short
test3 (vector signed short x, vector signed short y)
{
  return vec_eqv (x, y);
}

vector unsigned short
test6 (vector unsigned short x, vector unsigned short y)
{
  return vec_eqv (x, y);
}

/* { dg-final { scan-assembler-times "xxleqv" 3 } } */
