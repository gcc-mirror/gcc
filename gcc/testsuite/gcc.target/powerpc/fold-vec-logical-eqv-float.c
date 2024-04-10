/* Verify that overloaded built-ins for vec_eqv with float
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

#include <altivec.h>

vector float
test1 (vector float x, vector float y)
{
  return vec_eqv (x, y);
}

/* { dg-final { scan-assembler-times "xxleqv" 1 } } */
