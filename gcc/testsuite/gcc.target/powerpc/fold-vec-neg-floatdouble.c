/* Verify that overloaded built-ins for vec_neg with float and
   double inputs for VSX produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */


#include <altivec.h>

vector float
test1 (vector float x)
{
  return vec_neg (x);
}

vector double
test2 (vector double x)
{
  return vec_neg (x);
}

/* { dg-final { scan-assembler-times "xvnegsp" 1 } } */
/* { dg-final { scan-assembler-times "xvnegdp" 1 } } */
