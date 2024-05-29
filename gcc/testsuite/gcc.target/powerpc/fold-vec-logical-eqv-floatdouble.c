/* Verify that overloaded built-ins for vec_eqv with float and
   double inputs for VSX produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector float
test1 (vector float x, vector float y)
{
  return vec_eqv (x, y);
}

vector double
test2 (vector double x, vector double y)
{
  return vec_eqv (x, y);
}

/* { dg-final { scan-assembler-times "xxleqv" 2 } } */
