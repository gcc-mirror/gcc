/* { dg-options "-mdejagnu-cpu=power7" } */
/* { dg-require-effective-target powerpc_vsx } */

/* It's to verify no ICE here, ignore error messages about
   the required options for vec_neg here.  */
/* { dg-excess-errors "pr105271" } */

#include <altivec.h>

vector signed long long
test (vector signed long long x)
{
  return vec_neg (x);
}
