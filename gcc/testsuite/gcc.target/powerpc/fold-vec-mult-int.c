/* Verify that overloaded built-ins for vec_mul with int
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-mvsx" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector signed int
test3 (vector signed int x, vector signed int y)
{
  return vec_mul (x, y);
}

vector unsigned int
test6 (vector unsigned int x, vector unsigned int y)
{
  return vec_mul (x, y);
}

/* { dg-final { scan-assembler-times "\[ \t\]vmuluwm" 2 } } */

