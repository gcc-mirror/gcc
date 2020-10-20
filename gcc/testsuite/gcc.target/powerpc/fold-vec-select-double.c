/* Verify that overloaded built-ins for vec_sel with 
   double inputs for VSX produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */

#include <altivec.h>

vector double
test2_0 (vector double x, vector double y, vector bool long long z)
{
  return vec_sel (x, y, z);
}

vector double
test2_1 (vector double x, vector double y, vector unsigned long long z)
{
  return vec_sel (x, y, z);
}

/* { dg-final { scan-assembler-times "xxsel" 2 } } */
