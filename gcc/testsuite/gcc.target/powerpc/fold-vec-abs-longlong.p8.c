/* Verify that overloaded built-ins for vec_abs with long long
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -O2 -mdejagnu-cpu=power8" } */

#include <altivec.h>

vector signed long long
test3 (vector signed long long x)
{
  return vec_abs (x);
}

/* { dg-final { scan-assembler-times "vspltisw" 1 } } */
/* { dg-final { scan-assembler-times "vsubudm" 1 } } */
/* { dg-final { scan-assembler-times "vmaxsd" 1 } } */
