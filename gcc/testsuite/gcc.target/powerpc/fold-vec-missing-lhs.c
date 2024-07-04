/* This test is meant to verify that the gimple-folding does not
   occur when the LHS portion of an expression is missing.
   The intent of this test is to verify that we do not generate an ICE.
   This was noticed during debug of PR81317.  */

/* { dg-do compile } */
/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed short
test1_nolhs (vector bool short x, vector signed short y)
{
  vec_add (x, y);
  return vec_add (x, y);
}

vector signed short
test2_nolhs (vector signed short x, vector bool short y)
{
  vec_add (x, y);
  return vec_add (x, y);
}
