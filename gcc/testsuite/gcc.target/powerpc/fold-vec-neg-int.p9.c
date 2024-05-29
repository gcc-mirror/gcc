/* Verify that overloaded built-ins for vec_neg with int
   inputs produce the right code when -mcpu=power9 is specified.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2 -mdejagnu-cpu=power9 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector signed int
test1 (vector signed int x)
{
  return vec_neg (x);
}

/* { dg-final { scan-assembler-times "vnegw" 1 } } */

