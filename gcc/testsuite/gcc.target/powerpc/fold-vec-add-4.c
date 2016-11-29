/* Verify that overloaded built-ins for vec_add with long long
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-maltivec -mvsx -mpower8-vector" } */

#include <altivec.h>

vector signed long long
test1 (vector bool long long x, vector signed long long y)
{
  return vec_add (x, y);
}

vector signed long long
test2 (vector signed long long x, vector bool long long y)
{
  return vec_add (x, y);
}

vector signed long long
test3 (vector signed long long x, vector signed long long y)
{
  return vec_add (x, y);
}

vector unsigned long long
test4 (vector bool long long x, vector unsigned long long y)
{
  return vec_add (x, y);
}

vector unsigned long long
test5 (vector unsigned long long x, vector bool long long y)
{
  return vec_add (x, y);
}

vector unsigned long long
test6 (vector unsigned long long x, vector unsigned long long y)
{
  return vec_add (x, y);
}

/* { dg-final { scan-assembler-times "vaddudm" 6 } } */
