/* Verify that overloaded built-ins for vec_min with long long
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector" } */

#include <altivec.h>

vector signed long long
test3_min (vector signed long long x, vector signed long long y)
{
  return vec_min (x, y);
}

vector unsigned long long
test6_min (vector unsigned long long x, vector unsigned long long y)
{
  return vec_min (x, y);
}

vector signed long long
test3_max (vector signed long long x, vector signed long long y)
{
  return vec_max (x, y);
}

vector unsigned long long
test6_max (vector unsigned long long x, vector unsigned long long y)
{
  return vec_max (x, y);
}

/* { dg-final { scan-assembler-times "vminsd" 1 } } */
/* { dg-final { scan-assembler-times "vmaxsd" 1 } } */
/* { dg-final { scan-assembler-times "vminud" 1 } } */
/* { dg-final { scan-assembler-times "vmaxud" 1 } } */
