/* Verify that overloaded built-ins for vec_abs with int
   inputs produce the right code when -mcpu=power9 is specified.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-maltivec -O2 -mdejagnu-cpu=power9 -mvsx" } */

#include <altivec.h>

vector signed int
test1 (vector signed int x)
{
  return vec_abs (x);
}

/* { dg-final { scan-assembler-times "vnegw" 1 } } */
/* { dg-final { scan-assembler-times "vmaxsw" 1 } } */
