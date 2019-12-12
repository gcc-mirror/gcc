/* Verify that overloaded built-ins for vec_neg with int
   inputs produce the right code when -mcpu=power9 is specified.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-maltivec -O2 -mdejagnu-cpu=power9" } */

#include <altivec.h>

vector signed int
test1 (vector signed int x)
{
  return vec_neg (x);
}

/* { dg-final { scan-assembler-times "vnegw" 1 } } */

