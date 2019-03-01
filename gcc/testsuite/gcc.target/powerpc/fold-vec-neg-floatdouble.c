/* Verify that overloaded built-ins for vec_neg with float and
   double inputs for VSX produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mvsx -O2 -mdejagnu-cpu=power8" } */


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
