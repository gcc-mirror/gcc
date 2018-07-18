/* Verify that overloaded built-ins for vec_neg with long long
   inputs produce the right code.  */

/* vec_neg testcase, included by fold-vec-neg-longlong.p*.c */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -O2" } */

#include <altivec.h>

vector signed long long
test3 (vector signed long long x)
{
  return vec_neg (x);
}

