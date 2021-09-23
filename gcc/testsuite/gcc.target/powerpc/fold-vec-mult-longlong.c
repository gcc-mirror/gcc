/* Verify that overloaded built-ins for vec_mul with long long
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-maltivec -mvsx -mpower8-vector" } */
/* { dg-additional-options "-maix64" { target powerpc-ibm-aix* } } */

#include <altivec.h>

vector signed long long
test3 (vector signed long long x, vector signed long long y)
{
  return vec_mul (x, y);
}

vector unsigned long long
test6 (vector unsigned long long x, vector unsigned long long y)
{
  return vec_mul (x, y);
}

/* Power10 can generate the vmulld instruction even in 32-bit.  Before power10,
   we limit the code to lp64, since 32-bit cannot generate the mulld
   instruction.  */
/* { dg-final { scan-assembler-times {\mmulld\M}  4 { target { lp64 && { ! has_arch_pwr10 } } } } } */
/* { dg-final { scan-assembler-times {\mvmulld\M} 2 { target { has_arch_pwr10             } } } } */
